# interpolation.R
library(shiny)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(readr)
library(sf)
library(gstat)
library(terra)
library(tmap)

# Load static data outside the module scope
mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
  st_transform(crs = 3414)

weather_data <- read_csv("data/weather_data_cleaned.csv")
weather_data$Date <- as.Date(weather_data$Date)

station_coords <- weather_data %>%
  select(Station, Latitude, Longitude) %>%
  distinct()

# ---- MODULE UI ----
interpolationUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Spatial Interpolation of Weather Variables", style = "margin-top: 20px; margin-left: 10px;"),
    p("This tool estimates weather conditions across Singapore by interpolating measurements from individual weather stations.",
      style = "margin-left: 10px; margin-bottom: 30px; font-size: 16px; color: #333;"),
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(ns("variable"), "Select Variable:",
                               choices = c("Total Rainfall (mm)" = "Daily Rainfall Total (mm)",
                                           "Mean Temperature (°C)" = "Mean Temperature (°C)",
                                           "Maximum Temperature (°C)" = "Maximum Temperature (°C)",
                                           "Minimum Temperature (°C)" = "Minimum Temperature (°C)")),
                   
                   dateRangeInput(ns("date_range"), "Select Date Range:",
                                  start = as.Date("2024-01-01"),
                                  end = as.Date("2024-12-31"),
                                  min = as.Date("2020-01-01"),
                                  max = as.Date("2024-12-31"),
                                  format = "yyyy-mm-dd"),
                   
                   selectInput(ns("method"), "Interpolation Method:",
                               choices = c("Inverse Distance Weighted (IDW)" = "idw",
                                           "Kriging" = "kriging"),
                               selected = "idw"),
                   
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'idw'", ns("method")),
                     sliderTextInput(ns("nmax"), "Number of Nearest Stations (nmax):",
                                     choices = as.character(1:20), selected = "5", grid = TRUE)
                   ),
                   
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'kriging'", ns("method")),
                     selectInput(ns("kriging_model"), "Variogram Model:",
                                 choices = c("Spherical" = "Sph",
                                             "Exponential" = "Exp",
                                             "Gaussian" = "Gau"),
                                 selected = "Sph"),
                     
                     checkboxInput(ns("advanced"), "Show Advanced Controls", FALSE),
                     
                     conditionalPanel(
                       condition = sprintf("input['%s']", ns("advanced")),
                       numericInput(ns("nugget"), "Nugget:", value = 0),
                       numericInput(ns("psill"), "Partial Sill:", value = 10),
                       numericInput(ns("range"), "Range:", value = 5000)
                     )
                   ),
                   
                   actionButton(ns("generate"), "Generate Map")
      ),
      mainPanel(
        tmapOutput(ns("interpolationMap"), height = "700px")
      )
    )
  )
}



# ---- MODULE SERVER ----
interpolationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    interpolation_result <- eventReactive(input$generate, {
      withProgress(message = "Generating map...", value = 0.1, {
        start_date <- input$date_range[1]
        end_date <- input$date_range[2]
        
        filtered_data <- weather_data %>%
          filter(Date >= start_date & Date <= end_date)
        
        monthly_summary <- filtered_data %>%
          group_by(Station) %>%
          summarise(MONTHVAL = if (input$variable == "Daily Rainfall Total (mm)") {
            sum(.data[[input$variable]], na.rm = TRUE)
          } else {
            mean(.data[[input$variable]], na.rm = TRUE)
          }, .groups = "drop")
        
        rfdata <- left_join(monthly_summary, station_coords, by = "Station")
        rfdata_sf <- st_as_sf(rfdata, coords = c("Longitude", "Latitude"), crs = 4326) %>%
          st_transform(crs = 3414)
        
        grid <- terra::rast(mpsz2019, nrows = 300, ncols = 500)
        xy <- terra::xyFromCell(grid, 1:ncell(grid))
        coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = 3414)
        coop <- st_filter(coop, mpsz2019)
        
        if (input$method == "idw") {
          interp_model <- gstat(formula = MONTHVAL ~ 1,
                                locations = rfdata_sf,
                                nmax = as.numeric(input$nmax),
                                set = list(idp = 2))
        } else {
          vgm_model <- variogram(MONTHVAL ~ 1, rfdata_sf)
          model <- if (input$advanced) {
            vgm(psill = input$psill, model = input$kriging_model,
                range = input$range, nugget = input$nugget)
          } else {
            vgm(model = input$kriging_model)
          }
          
          fit <- tryCatch({
            fit.variogram(vgm_model, model = model)
          }, error = function(e) {
            showNotification(paste("❌ Variogram fitting failed:", e$message), type = "error")
            return(NULL)
          })
          
          if (is.null(fit)) return(NULL)
          
          interp_model <- gstat(formula = MONTHVAL ~ 1,
                                locations = rfdata_sf,
                                model = fit)
        }
        
        interp_result <- tryCatch({
          predict(interp_model, coop)
        }, error = function(e) {
          showNotification(paste("❌ Interpolation failed:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(interp_result)) return(NULL)
        
        interp_result$x <- st_coordinates(interp_result)[, 1]
        interp_result$y <- st_coordinates(interp_result)[, 2]
        interp_result$pred <- interp_result$var1.pred
        
        raster <- terra::rasterize(interp_result, grid, field = "pred", fun = "mean")
        names(raster) <- "pred"
        
        range_title <- paste(format(start_date, "%d %b %Y"), "to", format(end_date, "%d %b %Y"))
        
        palette <- if (input$variable == "Daily Rainfall Total (mm)") "brewer.blues" else "brewer.oranges"
        
        is_rainfall <- input$variable == "Daily Rainfall Total (mm)"
        legend_title <- if (is_rainfall) "Total Rainfall (mm)" else "Temperature (°C)"
        title_prefix <- if (is_rainfall) "Distribution of Total Rainfall" else "Distribution of Temperature"
        map_title <- paste(title_prefix, range_title, sep = ", ")
        
        list(
          raster = raster,
          title = map_title,
          palette = palette,
          legend_title = legend_title)
      })
    })
    
    output$interpolationMap <- renderTmap({
      req(interpolation_result())
      res <- interpolation_result()
      
      tmap_mode("plot")
      tm_shape(res$raster) +
        tm_raster(
          col = "pred",
          col.scale = tm_scale(values = res$palette),
          col_alpha = 0.8,
          col.legend = tm_legend(title = res$legend_title)
        ) +
        tm_shape(mpsz2019) +
        tm_borders() +
        tm_title(res$title) +
        tm_layout(legend.outside = TRUE)
    })
  })
}
