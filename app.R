install.packages(c(
  "gstat", "tmap", "shiny", "shinydashboard", "shinyWidgets", "tidyverse",
  "ggthemes", "plotly", "sf", "terra", "viridis", "ggHoriPlot", "ggstatsplot",
  "rstantools", "ISOweek", "DT", "nortest", "ggridges", "tsibble",
  "tsibbledata", "feasts", "fable", "fabletools", "fable.prophet",
  "RColorBrewer", "urca", "bslib", "parameters", "shinybusy"
))
renv::snapshot()

# Section 1: Set up ----
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, ggthemes, plotly, sf, terra, viridis, ggHoriPlot, ggstatsplot, rstantools, ISOweek, DT, nortest, ggridges, tsibble, tsibbledata, feasts, fable, fabletools, fable.prophet, RColorBrewer, urca, bslib, parameters, shinybusy)

# Section 1.1: Variables and Functions ----
## Import data 

# Load weather_data_cleaned.csv
weather_data <- read_csv("data/weather_data_cleaned.csv") %>%
  mutate(
    Date = as.Date(Date),
    Station = as.factor(Station),
    Latitude = Latitude,
    Longitude = Longitude
  )

# Define variables for selection
variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")
variables_select <- c(
  "Total Rainfall (mm)" = "Daily Rainfall Total (mm)",
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
  "Maximum Temperature (°C)" = "Maximum Temperature (°C)"
)

# Convert to tsibble
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)

# Station list
Station <- weather_tsbl %>% distinct(factor(Station))

# Forecast model choices
ForecastTS_model_choices <- list(
  "STL Naive" = "STL_Naive",
  "STL ARIMA" = "STL_ARIMA",
  "STL ETS" = "STL_ETS",
  "AUTO ARIMA" = "AUTO_ARIMA",
  "AUTO Prophet" = "AUTO_Prophet",
  "AUTO ETS" = "AUTO_ETS"
)

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")
variables_select <- c("Total Rainfall (mm)" = "Daily Rainfall Total (mm)", "Mean Temperature (°C)" = "Mean Temperature (°C)", "Minimum Temperature (°C)" = "Minimum Temperature (°C)" , "Maximum Temperature (°C)" = "Maximum Temperature (°C)")

## Used for Time Series module
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)
Station <- weather_tsbl %>% distinct(factor(Station))
ForecastTS_model_choices <- list("STL Naive" = "STL_Naive","STL ARIMA" = "STL_ARIMA","STL ETS" = "STL_ETS","AUTO ARIMA" = "AUTO_ARIMA","AUTO Prophet" = "AUTO_Prophet","AUTO ETS" = "AUTO_ETS")

## Used for Spatial Interpolation module
GS_model_choices <- c("Nug", "Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Pen","Per","Wav","Hol","Log","Pow","Spl")
# Create polygon boundary from station locations
station_points <- weather_data %>%
  select(Station, Latitude, Longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 3414)

# Create convex hull around station points as substitute for boundary
mpsz2019 <- st_convex_hull(st_union(station_points))
mpsz2019 <- st_sf(geometry = mpsz2019)

## Function to calculate ValueToPlot based on  selected variable.
## Used in Time Series and Spatial Interpolation modules
calculateValueToPlot <- function(data, var) {
  if (grepl("Rainfall", var)) {round(sum(data[[var]], na.rm = TRUE),2)} else if (grepl("Temperature", var)) {round(mean(data[[var]], na.rm = TRUE), 2)}
}

## Function to prepare data for CDA Across Stations tab
CDA_AS_prepareVariableData <- function(selected_var, time_resolution, selected_date, selected_stations, weather_data){
  if (time_resolution == "Month") {
    selected_month <- format(as.Date(selected_date), "%m")
    selected_year <- format(as.Date(selected_date), "%Y")
    variable_data <- weather_data %>% filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month), Station %in% selected_stations)
  } else if (time_resolution == "Year") {
    selected_year <- as.character(format(selected_date, "%Y"))
    variable_data <- weather_data %>% filter(format(Date, "%Y") == selected_year, Station %in% selected_stations)
  } 
  return(list(selected_var = selected_var, variable_data = variable_data, time_resolution = time_resolution, selected_date = selected_date, selected_stations = selected_stations))
}

## Function to prepare data for TS module
TS_prepareVariableData <- function(selected_var, char_startDate, char_endDate, selected_station, time_resolution, weather_tsbl) {
  variable_data <- weather_tsbl %>%
    filter_index(char_startDate ~ char_endDate) %>%
    filter(Station == selected_station)
  if (time_resolution == "Day") {
    variable_data <- variable_data %>% mutate(ValueToPlot = .data[[selected_var]])
  } else if (time_resolution == "Week") {
    variable_data <- variable_data %>%
      group_by_key() %>%
      index_by(year_week = ~ yearweek(.)) %>%
      summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
      mutate(Date = floor_date(as.Date(year_week), unit = "week"))
  }
  time_title <- switch(time_resolution, "Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
  var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {if(time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
  title <- paste(time_title, var_title, "for", selected_station)
  return(list(variable_data = variable_data, title = title, char_startDate = char_startDate, char_endDate = char_endDate, selected_station = selected_station, time_resolution = time_resolution))
}

## Function to prepare variable data and plot elements. Used for Spatial Interpolation module
GS_prepareVariableData <- function(selected_var, time_resolution, selected_time, weather_data) {
  variable_data <- NULL
  if (time_resolution == "Day") {
    selected_date <- as.Date(selected_time)
    variable_data <- weather_data %>% filter(Date == selected_date)
  } else if (time_resolution == "Month") {
    selected_month <- format(as.Date(selected_time), "%m")
    selected_year <- format(as.Date(selected_time), "%Y")
    variable_data <- weather_data %>% filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month))
  } else if (time_resolution == "Year") {
    selected_year <- as.character(format(selected_time, "%Y"))
    variable_data <- weather_data %>%filter(format(Date, "%Y") == selected_year)
  }
  # Group and summarise data based on the selected variable
  if (!is.null(variable_data)) {
    variable_data <- variable_data %>%
      group_by(Station, Latitude, Longitude) %>%
      summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop')
  } else {variable_data <- NULL}
  
  # Add legend_title to variable
  legend_title <- if(grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if(grepl("Temperature", selected_var)) {"Temperature (°C)"}
  
  # Update variable_data before converting to sf
  if (!is.null(variable_data)) {
    variable_data[[legend_title]] <- variable_data$ValueToPlot
    variable_data_sf <- st_as_sf(variable_data, coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(crs = 3414)
  } else {variable_data_sf <- NULL}
  time_title <- switch(time_resolution, "Day" = "Daily", "Month" = "Monthly", "Year" = "Yearly", NA)
  var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {if(time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
  date_title <- format(as.Date(selected_time), switch(time_resolution,"Day" = "%d %B %Y","Month" = "%B %Y","Year" = "%Y",NA))
  main_title <- paste(time_title, var_title, "for\n", date_title)
  
  palette <- if (grepl("Rainfall", selected_var)) {"Blues"} else if (grepl("Temperature", selected_var)) {"YlOrRd"}
  
  return(list(variable_data = variable_data, variable_data_sf = variable_data_sf, legend_title = legend_title, main_title = main_title, palette = palette, var_title = var_title))
}

# Function to get raster layer
GS_rasterlayer <- function(res){
  # Specify resolution
  bbox <- as.list(st_bbox(mpsz2019))
  nrows =  (bbox$ymax - bbox$ymin)/res
  ncols = (bbox$xmax - bbox$xmin)/res
  # Create raster layer, 'grid'
  grid <- rast(mpsz2019, nrows = nrows, ncols = ncols)
  # Generate coordinates for each cell of the raster
  xy <- xyFromCell(grid,1:ncell(grid))
  # Converting coordinates of raster into a spatial (sf) object
  coop <- st_as_sf(as.data.frame(xy), coords = c("x","y"), crs = st_crs(mpsz2019))
  # Filter to only only includes points within mpsz2019
  coop <- st_filter(coop, mpsz2019) 
  return(list(grid = grid, coop = coop))
}

# Section 2: Header and sidebar ----
header <- dashboardHeader(title = "Visual Analytics for Singapore's Weather",
                          titleWidth = 450)

sidebar <- dashboardSidebar(
  width = 170,
  tags$head(
    tags$style(HTML(
      ".sidebar-menu > li > a, .sidebar-menu .treeview-menu > li > a {
        white-space: normal; 
        line-height: 1.2;
        font-size: 12px;
      }
      /* Custom styles for the image */
      .sidebar-logo {
        padding: 10px;
        text-align: center; /* Center the image */
      }
      .sidebar-logo img {
        width: 80%; /* Adjust the image size */
        margin-bottom: 10px; /* Extra space below the image */
      }"
    ))
  ),
  # Use div with custom class for styling and placing the image
  div(class = "sidebar-logo",
      tags$img(src = "sunny.png", alt = "Logo")
  ),
  sidebarMenu(
    menuItem("Landing Page", tabName = "LandingPage", icon = icon("home")),
    menuItem("Exploratory & Confirmatory Data Analysis", tabName = "EDACDA", icon = icon("temperature-half")),
    menuItem("Univariate Forecasting", tabName = "Univariate", icon = icon("chart-line"),
             menuSubItem("Exploratory Time Series", tabName = "ExploreTS"),
             menuSubItem("Time Series Decomposition", tabName = "DecomposeTS"),
             menuSubItem("Forecasting", tabName = "ForecastTS")
    ),
    menuItem("Spatial Interpolation", tabName = "Geospatial", icon = icon("globe-asia"))
  )
)
# Section 3: CDA UI ----
CDAUI <- fluidPage(
  fluidRow(tabBox(
    title = "", width = 12, id = "CDA_tab", height = "250px",
    tabPanel("Compare across Stations",
             fluidRow(
               box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
                   selectInput("CDA_AS_selected_var", "Choose variable", variables, selected = "Mean Temperature (°C)", multiple = FALSE),
                   radioButtons("CDA_AS_time_resolution", label = "Select time resolution", c("Month", "Year")),
                   uiOutput("CDA_AS_dynamic_time_resolution"),
                   checkboxGroupInput("CDA_AS_selected_stations", "Select Station (s)", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1])
               ),
               tabBox(title = "", width = 10, id = "CDA_AS_tab", height = "250px",
                      tabPanel("Check Normality",
                               actionButton("CDA_AS_checknormality_button", "Run Normality Checks"),
                               plotOutput("CDA_AS_checknormality_plot"),
                               tableOutput("CDA_AS_checknormality_results")
                      ),
                      tabPanel("Run Statistic Test",
                               fluidRow(column(2, 
                                               selectInput("CDA_AS_selectedStatApproach", "Statistical Approach", choices = c("parametric", "nonparametric", "robust", "bayes"), selected = "nonparametric", multiple = FALSE),
                                               selectInput("CDA_AS_selectedConflevel", "Confidence Level", choices = c("90%"=0.90,"95%"=0.95, "99%"=0.99),selected ="0.95", multiple = FALSE),
                                               selectInput("CDA_AS_plotType", "Plot Type",choices = c("Boxviolin" = "boxviolin", "Box" = "box", "Violin" = "violin"),selected = "boxviolin",multiple = FALSE),
                                               textInput("CDA_AS_plot_title","Plot Title", placeholder = "Enter plot title"),
                                               actionButton("CDA_AS_plot_button", "Run test"),
                                               textInput("CDA_AS_Insights","Insights", placeholder = "Enter your insights"),
                                               actionButton("CDA_AS_Insights_button", "Save insights")
                               ),
                               column(10, plotlyOutput("CDA_AS_plot"), 
                                      uiOutput("CDA_AS_test_results"),
                                      verbatimTextOutput("CDA_AS_Insights_Output")
                               ))
                      )
               )
             )
    ),
    tabPanel("Compare across Time",
             fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
                          selectInput("CDA_AT_selected_var", "Choose variable", variables, selected = "Mean Temperature (°C)", multiple = FALSE),
                          selectInput("CDA_AT_selected_station", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1],  multiple = FALSE),
                          selectInput("CDA_AT_time_resolution", label = "Compare across", c("Years", "Months", "Months for a specified year", "Months of different years"), selected = NULL, multiple = FALSE),
                          uiOutput("CDA_AT_dynamic_time_resolution")
             ),
             tabBox(title = "", width = 10, id = "CDA_AT_tab", height = "250px",
                    tabPanel("Check Normality",
                             actionButton("CDA_AT_checknormality_button", "Run Normality Checks"),
                             plotOutput("CDA_AT_checknormality_plot"),
                             tableOutput("CDA_AT_checknormality_results")
                    ),
                    tabPanel("Run Statistic Test",
                             fluidRow(column(2, selectInput("CDA_AT_selectedStatApproach", "Statistical Approach", choices = c("parametric", "nonparametric", "robust", "bayes"),selected = "nonparametric", multiple = FALSE),
                                             selectInput("CDA_AT_selectedConflevel", "Confidence Level", choices = c("90%"=0.90,"95%"=0.95, "99%"=0.99),selected ="0.95", multiple = FALSE),
                                             selectInput("CDA_AT_plotType", "Plot Type",choices = c("Boxviolin" = "boxviolin", "Box" = "box", "Violin" = "violin"),selected = "boxviolin",multiple = FALSE),
                                             textInput("CDA_AT_plot_title","Plot Title", placeholder = "Enter plot title"),
                                             actionButton("CDA_AT_plot_button", "Run test"),
                                             textInput("CDA_AT_Insights","Insights", placeholder = "Enter your insights"),
                                             actionButton("CDA_AT_Insights_button", "Save insights")
                             ),
                             column(10, plotlyOutput("CDA_AT_plot"), 
                                    uiOutput("CDA_AT_test_results"),
                                    verbatimTextOutput("CDA_AT_Insights_Output")
                             )
                             )
                    )
                    
             )
             )
    )
  )
  )
)

# Section 4.1: ExploreTS UI ----
ExploreTSUI <- fluidPage(
  fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
               selectInput("ExploreTS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
               uiOutput("ExploreTS_dynamic_time_resolution"),
               checkboxGroupInput("ExploreTS_selectstation", "Select Station", choices = unique(weather_tsbl$Station),  selected = unique(weather_tsbl$Station)[1]),
               dateInput("ExploreTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-12-31", startview ="year"),
               dateInput("ExploreTS_endDate", "End Date", value = "2023-12-31", min = "2021-01-02", max = "2023-12-31", startview ="year"),
               uiOutput("ExporeTS_Date_Instructions")
  ),
  tabBox(title = "", width = 10,id = "ExploreTS_tab", height = "250px",
         tabPanel("Line graph",
                  plotlyOutput("ExploreTS_timeSeriesPlot"),
                  DT::dataTableOutput("ExploreTS_timeSeriesPlot_DataTable")
                  
         ),
         tabPanel("Horizon plot",
                  plotOutput("ExploreTS_HoriPlot")
         )
  )
  )
)

# Section 4.2: DecomposeTS UI ----
DecompTSUI <- fluidPage(
  fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
               selectInput("DecompTS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
               uiOutput("DecompTS_dynamic_time_resolution"),
               selectInput("DecompTS_selected_station", "Select Station", choices = unique(weather_tsbl$Station), selected = unique(weather_tsbl$Station)[1]),
               uiOutput("DecompTS_startDate_ui"),
               HTML("<div style='margin-top: 15px; margin-bottom: 15px;'>
  <strong>End Date (fixed)</strong><br>
  <div style='padding: 5px 10px; margin-top: 5px; display: inline-block; width: auto; color: #808080;'>
    2023-12-31
  </div>
</div>"),
               uiOutput("DecompTS_Date_Instructions"),
               HTML("<div style='margin-bottom: 10px;'></div>")
  ),
  
  
  tabBox(title = "", width = 10,id = "DecompTS_tab", height = "250px",
         tabPanel("ACF & PACF",
                  fluidRow(
                    column(3, title = "parameters",
                           uiOutput("DecompTS_dynamiclags")), 
                    column(9, plotlyOutput("DecompTS_ACFPlot", width = "100%", height = "300px"),
                           plotlyOutput("DecompTS_PACFPlot", width = "100%", height = "300px"))
                  )
         ),
         tabPanel("STL Decomposition",
                  fluidRow(
                    column(3, title = "parameters",
                           radioButtons("DecompTS_chooseautoSTL", label = "Use Auto STL?" ,c("Yes", "No")),
                           uiOutput("DecompTS_dynamic_autoSTL")),
                    column(9,plotlyOutput("DecompTS_STLPlot",width = "100%", height = "600px"))
                  )
         )
  )
  )
)

# Section 4.3: ForecastTS UI ----
ForecastTSUI <- fluidPage(
  # Row 1
  fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
               selectInput("ForecastTS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
               selectInput("ForecastTS_selected_station", "Select Station", choices = unique(weather_tsbl$Station), selected = unique(weather_tsbl$Station)[1]),
               uiOutput("ForecastTS_dynamic_time_resolution"),
               uiOutput("ForecastTS_startDate_ui"),
               HTML("<div style='margin-top: 15px; margin-bottom: 15px;'>
  <strong>End Date (fixed)</strong><br>
  <div style='padding: 5px 10px; margin-top: 5px; display: inline-block; width: auto; color: #808080;'>
    2023-12-31
  </div>
</div>"),
               uiOutput("ForecastTS_Date_Instructions"),
               HTML("<div style='margin-bottom: 10px;'></div>"),
               checkboxGroupInput("ForecastTS_selected_models", "Select Forecasting Models", choices = ForecastTS_model_choices),
               uiOutput("ForecastTS_dynamic_chooseautoSTL"),
               uiOutput("ForecastTS_dynamic_model_parameters"),
               sliderInput("ForecastTS_train_test_split", "Select Train-Test Split", min = 0.6, max = 1, value = 0.8, step = 0.1)
  ),
  tabBox(title = "", width = 10, id = "ForecastTS_tab", height = "250px",
         tabPanel("Model Calibration",
                  actionButton("ForecastTS_build_model", "Build Model"),
                  plotlyOutput("ForecastTS_forecast_validation_plot"),
                  fluidRow(column(6,plotlyOutput("ForecastTS_residual_plot", width= "100%", height = "280px")),
                           column(6, DT::dataTableOutput("ForecastTS_buildModel_DataTable"))
                  )
         ),
         tabPanel("Forecast Result",
                  fluidRow(
                    column(2,uiOutput("ForecastTS_dynamic_forecast_period"),
                           actionButton("ForecastTS_future_forecast", "Forecast")),
                    column(10, 
                           # uiOutput("ForecastTS_future_forecast_result")
                           plotlyOutput("ForecastTS_future_forecast_plot"),
                           DT::dataTableOutput("ForecastTS_future_forecast_DataTable")
                    )
                  )
         )
  )
  )
)


# Section 5: Geospatial UI ----

GeospatialUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("GS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
        selectInput("GS_time_resolution", "Time resolution", c("Day", "Month", "Year"), selected = "Month", multiple = FALSE),
        uiOutput("GS_dynamic_time_resolution")
    ),
    tabBox(
      title = "", width = 10, id = "Geospatial_tab", height = "auto",
      # The id lets us use input$Geospatial_tab on the server to find the current tab
      tabPanel("Map of stations",
               actionButton("GS_updatetmap", "Update map"), 
               uiOutput("GS_tmap_title"),
               tmapOutput("GS_tmap"),
               DT::dataTableOutput("GS_tmap_DataTable")
               
      ),
      tabPanel("Inverse Distance Weighted Interpolation Method",
               fluidRow(
                 column(3, 
                        sliderInput("GS_IDW_res", "res", min = 100, max = 300, value = 100, step = 50),
                        sliderInput("GS_IDW_nmax" , "nmax", min = 1, max = 10, value = 5),
                        actionButton("GS_updateIDW", "Show Result")
                 ),
                 column(9,plotOutput("GS_IDW_map", width = "100%", height = "600px"))
               )
      ),
      tabPanel("Ordinary Kriging Method",
               fluidRow(
                 column(2, 
                        sliderInput("GS_OK_res", "res", min = 100, max = 300, value = 100, step = 50),
                        selectInput("GS_OK_model" , "model", GS_model_choices, selected = "Gau"),
                        sliderInput("GS_OK_psill", "psill", min = 0.5, max = 10, value = 0.5, step = 0.5),
                        uiOutput("GS_OK_range_reactiveUI"),
                        sliderInput("GS_OK_nugget", "nugget", min = 0.1, max = 10, value = 0.1, step = 0.1),
                        actionButton("GS_updateOK", "Show Result")
                 ),
                 column(10,
                        fluidRow(
                          box(title =  tags$h4("Experimental and Fitted Variograms"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                              column(6,plotOutput("GS_OK_variogram", height = "400px")),
                              column(6,plotOutput("GS_OK_fitted_variogram", height = "400px"))
                          )
                        ),
                        fluidRow(uiOutput("GS_OK_result_title")),
                        fluidRow(
                          column(6,plotOutput("GS_OK_map",width = "100%", height = "500px")),
                          column(6,plotOutput("GS_OK_prediction_variance",width = "100%", height = "500px")))
                        
                 )
               )
      )
    )
  )
)

# Section 6: LandingPage UI ----
# Define UI
LandingPageUI <- fluidPage(
  fluidRow(
    column(width = 6,
           h2("A Visual Exploration Tool for Singapore's Weather"),
           p("Understanding Singapore's changing weather patterns is crucial, yet current tools for visualizing historical weather data are limited, often static, and lack depth. To fill this gap, we developed this interactive R Shiny application. Use this tool to explore and analyse Singapore's weather (2021-2023)!"),
           h3("Overview of modules in app"),
           tags$img(src = "overview.png", width = "100%"),
           p(strong("Exploratory & Confirmatory Data Analysis:"), 
             " Enables analysis of climate variables across stations and time periods, using statistical tests and interactive plots to identify weather patterns."),
           p(strong("Univariate Forecasting:"), 
             " Combines exploratory analysis, decomposition, and advanced forecasting with seasonal adjustment for accurate weather predictions across multiple stations."),
           p(strong("Spatial Interpolation:"), 
             " Generates isohyet/isotherm maps from sparse data, using interpolation techniques to estimate conditions at unmonitored locations, informed by data from Singapore's weather stations."),
           # p("The app consists of three key modules, each enabling the user to conduct a type of visual analysis with the dataset. The Exploratory & Confirmatory Data Analysis module allows users to conduct comparative analyses of weather data over different spatial and temporal scales. The Univariate Forecasting module provides a comprehensive toolkit of different forecasting models. The Spatial Interpolation module allows users to generate isohyet or isotherm maps to estimate rainfall/temperature at unmonitored locations. "),
           p("Click ", a("here", href = "https://isss608-group9-weatheranalytics.netlify.app/shinyapp/userguide.pdf", target = "_blank"), " to view the user guide.")
           
    ),
    column(3,
           uiOutput("hottestTemperatureInfo"),
           HTML("<div style='margin-bottom: 10px;'></div>"),
           uiOutput("wettestMonthInfo")),
    column(3,
           uiOutput("coolestTemperatureInfo"),
           HTML("<div style='margin-bottom: 10px;'></div>"),
           uiOutput("averageNumberOfRainyDaysInfo"))
  ),
  fluidRow(
    column(6, 
           h3("About the Dataset: Singapore Climate Records (2021-2023)"),
           p("The dataset comprises historical daily records of rainfall and temperature across 11 locations in Singapore, spanning from 2021 to 2023."),
           dataTableOutput("landingPageDataTable")),
    column(6, tmapOutput("stationsTmap"))
    
  )
)
# Section 7: Dashboard Body and UI ----
body <- dashboardBody(
  add_busy_spinner(spin = "fading-circle"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Merriweather+Sans:ital,wght@0,300..800;1,300..800&display=swap"),
    tags$style(HTML("
  body{
        font-family: 'Merriweather Sans', sans-serif;
        font-size: 14px; /* Set the default font size */
  }
  h1 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 2em; /* 36px if the base size is 16px */
  font-weight: 700; /* Bold */
  }
  h2 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 1.75em; /* 28px if the base size is 16px */
  font-weight: 700; /* Bold */
  }
  h3 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 1.25em; 
  font-weight: 600; /* Semi-Bold */
  }
  h4 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 1em; 
  font-weight: 600; /* Semi-Bold */
  }


      /* Customize the main header and navbar with a bold font */
      .skin-blue .main-header .logo, .skin-blue .main-header .logo:hover, .skin-blue .main-header, .skin-blue .main-header .navbar {
        background-color: #3D8DBC !important;
        font-family: 'Merriweather Sans', sans-serif;
        font-weight: 700; /* Bold */
      }
        .skin-blue .main-header .logo:hover {
          background-color: #3D8DBC;
          font-family: 'Merriweather Sans', sans-serif;
        }
        .skin-blue .main-header, .skin-blue .main-header .navbar {
      background-color: #3D8DBC!important;
        font-family: 'Merriweather Sans', sans-serif;
        }

      "))
  ),
  tabItems(
    tabItem(tabName = "LandingPage", LandingPageUI),
    tabItem(tabName = "EDACDA", h2("Exploratory & Confirmatory Data Analysis"), CDAUI),
    tabItem(tabName = "Univariate"),
    tabItem(tabName = "ExploreTS", h2("Exploring timeseries across stations"), ExploreTSUI),
    tabItem(tabName = "DecomposeTS",h2("Time Series Decomposition and ACF PACF plots"), DecompTSUI),
    tabItem(tabName = "ForecastTS",h2("Forecasting with different models"), ForecastTSUI),
    tabItem(tabName = "Geospatial",h2("Spatial Interpolation"), GeospatialUI)
  )
)

ui <- dashboardPage(header, sidebar, body)

# Section 8: Server code ----
server <- function(input, output, session) {
  
  # Landing Page ----
  
  output$landingPageDataTable <-DT::renderDataTable({
    
    tbl <- weather_data 
    tbl <- tbl %>% select(c(Station, Date, `Daily Rainfall Total (mm)`, `Mean Temperature (°C)`, `Maximum Temperature (°C)`, `Minimum Temperature (°C)`,  Latitude, Longitude))
    tbl$Station <-  as.factor(tbl$Station)
    datatable(tbl, 
              class= "hover",
              rownames = FALSE,
              width="80%", 
              filter = 'top',
              options = list(pageLength = 5, scrollX = TRUE))
  })
  
  
  output$stationsTmap <- renderTmap({
    
    unique_stations <- weather_data %>% 
      select(Station, Latitude, Longitude) %>% 
      distinct(Latitude, Longitude, .keep_all = TRUE)
    
    stations_sf <- st_as_sf(unique_stations, coords = c("Longitude", "Latitude"), crs = 4326)
    
    tm <-tm_basemap(server = "OpenStreetMap") + 
      tm_shape(stations_sf) +
      tm_dots(size = 0.2, col = '#9D6381', border.col = "#FDECEF",               # Set border color of dots to black
              border.lwd = 1) +  # Adjust size as needed
      tm_layout(title = "Station Locations in Singapore") +
      tm_view(set.view = c(lon = 103.8198, lat = 1.3521, zoom = 11)) # Centering on Singapore
    
    tm
    
  })
  
  output$hottestTemperatureInfo <- renderUI({
    # Find the row with the hottest temperature
    hottest_row <- weather_data[which.max(weather_data$`Maximum Temperature (°C)`), ]
    
    # Extract the hottest temperature, station, and date
    hottest_temperature <- hottest_row$`Maximum Temperature (°C)`
    station <- hottest_row$Station
    date <- hottest_row$Date
    # Define a nice light red color for the box fill
    box_fill_color <- "#ffcccc"
    
    tags$div(class = "panel panel-default", 
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      # Use a div with display flex to place the image next to the temperature
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "hot.png", height = "50px"), # Image from www folder
                               tags$h2(paste0(hottest_temperature, " °C"), style = "margin-left: 10px;") # Temperature next to the image
                      ),
                      tags$h3("Hottest Temperature Recorded:", style = "margin-top: 10px; text-align: center;") # "Hottest Temperature" text below
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Station:</strong> ", station, "</p>",
                                  "<p><strong>Date:</strong> ", date, "</p>")) # Station and date info in the panel body
             )
    )
  })
  
  output$coolestTemperatureInfo <- renderUI({
    # Find the row with the coolest temperature
    coolest_row <- weather_data[which.max(weather_data$`Minimum Temperature (°C)`), ]
    
    # Extract the coolest temperature, station, and date
    coolest_temperature <- coolest_row$`Minimum Temperature (°C)`
    station <- coolest_row$Station
    date <- coolest_row$Date
    # Define a nice light blue color for the box fill
    box_fill_color <- "#D7F9FF"
    
    # Create a box using Shiny's tags for custom HTML content
    tags$div(class = "panel panel-default",
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      # Use a div with display flex to place the image next to the temperature
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "cold.png", height = "50px"), # Image from www folder
                               tags$h2(paste0(coolest_temperature, " °C"), style = "margin-left: 10px;") # Temperature next to the image
                      ),
                      tags$h3("Coolest Temperature Recorded", style = "margin-top: 10px; text-align: center;") # "Coolest Temperature" text
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Station:</strong> ", station, "</p>",
                                  "<p><strong>Date:</strong> ", date, "</p>")) # Station and date info in the panel body
             )
    )
  })
  
  output$wettestMonthInfo <-renderUI({
    # Ensure weather_data$`Daily Rainfall Total (mm)` is numeric
    weather_data$`Daily Rainfall Total (mm)` <- as.numeric(as.character(weather_data$`Daily Rainfall Total (mm)`))
    
    # Aggregate rainfall by Year and Month to find total monthly rainfall
    monthly_rainfall <- weather_data %>%
      group_by(Year, Month) %>%
      summarise(Total_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE)) %>%
      ungroup()
    
    # Find the month with the highest total rainfall
    rainiest_month_info <- monthly_rainfall[which.max(monthly_rainfall$Total_Rainfall), ]
    
    
    # Assuming rainiest_month_info has been calculated as shown above
    
    # Extract information
    total_rainfall <- rainiest_month_info$Total_Rainfall
    year <- rainiest_month_info$Year
    month <- month.name[rainiest_month_info$Month] # Convert numeric month to name
    
    # Define a nice light blue color for the box fill
    box_fill_color <- "#A8D5E2"
    
    # Create a box using Shiny's tags for custom HTML content
    tags$div(class = "panel panel-default", 
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      # Flex display to place the image next to the heading
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "rain.png", height = "50px"), # Image from www folder
                               tags$h2(paste0(month, " ", year), style = "margin-left: 10px;") # Month and Year next to the image
                      ),
                      tags$h3("Rainiest Month Recorded", style = "margin-top: 10px; text-align: center;") # Heading text below image and date
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Total Rainfall (mm):</strong> ", total_rainfall, " mm</p>",
                                  "<p>(Across all stations in Singapore)")) # Rainfall info in the panel body
             )
    )
  })
  
  
  output$averageNumberOfRainyDaysInfo <-renderUI({
    # Ensure weather_data$`Daily Rainfall Total (mm)` is numeric
    weather_data$`Daily Rainfall Total (mm)` <- as.numeric(as.character(weather_data$`Daily Rainfall Total (mm)`))
    
    # Identify rainy days (>0 mm rainfall)
    weather_data <- mutate(weather_data, RainyDay = ifelse(`Daily Rainfall Total (mm)` > 0, 1, 0))
    
    # Aggregate to count rainy days per station and month
    rainy_days_per_month_station <- weather_data %>%
      group_by(Year, Month, Station) %>%
      summarise(RainyDays = sum(RainyDay, na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
    
    # Find the station and month with the highest number of rainy days
    max_rainy_days_info <- rainy_days_per_month_station[which.max(rainy_days_per_month_station$RainyDays), ]
    
    # Extract information
    total_rainy_days <- max_rainy_days_info$RainyDays
    year <- max_rainy_days_info$Year
    month <- month.name[max_rainy_days_info$Month] # Convert numeric month to name
    station_with_most_rainy_days <- max_rainy_days_info$Station
    
    # Define a nice light blue color for the box fill
    box_fill_color <- "#D3D4D9"
    
    # Create a box using Shiny's tags for custom HTML content
    tags$div(class = "panel panel-default", 
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "kid.png", height = "50px"), # Image from www folder
                               tags$h2(total_rainy_days, style = "margin-left: 10px;")
                      ),
                      tags$h3("Highest number of Rainy Days", style = "margin-top: 10px; text-align: center;")
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Station:</strong> ", station_with_most_rainy_days, "</p>",
                                  "<p><strong>Month & Year:</strong> ", month, " ", year, "</p>"))
             )
    )
  })
  
  
  
  
  # CDA: Compare Across Stations ----
  
  # 1. Dynamic UI: CDA_AS_dynamic_time_resolution
  output$CDA_AS_dynamic_time_resolution <- renderUI ({
    if(input$CDA_AS_time_resolution == "Month") {
      airMonthpickerInput("CDA_AS_selected_date", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$CDA_AS_time_resolution == "Year") {
      airYearpickerInput("CDA_AS_selected_date", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  # 2. Normality Plot
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AS_checknormality_data_prep <- eventReactive(input$CDA_AS_checknormality_button, {
    req(length(input$CDA_AS_selected_stations) > 0)  # Ensure at least one station is selected    
    
    # Prepare data 
    result <- CDA_AS_prepareVariableData(input$CDA_AS_selected_var, input$CDA_AS_time_resolution, input$CDA_AS_selected_date, input$CDA_AS_selected_stations, weather_data)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    time_resolution <- result$time_resolution
    selected_stations <- input$selected_stations
    selected_date <- input$CDA_AS_selected_date # Take directly from input
    
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {selected_var}
    time_title <- if (input$CDA_AS_time_resolution == "Month") {paste(format(as.Date(selected_date), "%B"), format(as.Date(selected_date), "%Y"))} else if (input$CDA_AS_time_resolution == "Year") {as.character(format(selected_date, "%Y"))}
    title <- paste("Distribution of", var_title, "for", time_title)
    
    
    list(variable_data = variable_data, selected_var = selected_var, time_resolution = time_resolution, selected_stations = selected_stations, selected_date = selected_date, title = title)
  })
  
  
  ## Output normality plot
  output$CDA_AS_checknormality_plot <- renderPlot({
    
    result <- CDA_AS_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    time_resolution <- result$time_resolution
    selected_date <- result$selected_date
    title <- result$title
    
    ggplot(data = variable_data,
           aes(x = !!var_symbol, y = Station)) +
      geom_density_ridges(fill =     if (grepl("Rainfall", selected_var)) {"lightblue"} else if (grepl("Temperature", selected_var)) {"#FEC26B"}, alpha = 0.9) +
      labs(title = title) +
      theme_ridges()+
      theme(legend.position = "none", axis.title.y = element_blank())   
  })
  
  ## Output table of normality test results 
  output$CDA_AS_checknormality_results <- renderTable({
    
    result <- CDA_AS_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    
    ad_results <- variable_data %>%
      group_by(Station) %>%
      summarise_at(vars(!!var_symbol), list(
        `Anderson-Darling Statistic` = ~ad.test(.)$statistic,
        `Anderson-Darling p.value` = ~ad.test(.)$p.value,
        `Shapiro-Wilk Statistic` = ~shapiro.test(.)$statistic,
        `Shapiro-Wilk p.value` = ~shapiro.test(.)$p.value
      )) %>%
      mutate(across(where(is.numeric), ~format(round(., 4), nsmall = 4))) %>%
      ungroup()
    ad_results
  })
  
  #3. Comparison Plot
  
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AS_caption_reactive <- reactiveValues() # Initializes a reactive value FOR CAPTION
  
  CDA_AS_data_prep <- eventReactive(input$CDA_AS_plot_button, {
    req(length(input$CDA_AS_selected_stations) > 0)  # Ensure at least one station is selected    
    
    # Prepare data 
    result <- CDA_AS_prepareVariableData(input$CDA_AS_selected_var, input$CDA_AS_time_resolution, input$CDA_AS_selected_date, input$CDA_AS_selected_stations, weather_data)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    var_symbol <- rlang::sym(selected_var)
    time_resolution <- result$time_resolution
    # selected_stations <- input$selected_stations
    selected_date <- input$CDA_AS_selected_date # Take directly from input
    title <- input$CDA_AS_plot_title # Take directly from input
    selectedStatApproach <- input$CDA_AS_selectedStatApproach
    selectedConflevel <- input$CDA_AS_selectedConflevel
    plotType <- input$CDA_AS_plotType
    
    p<-ggbetweenstats(data = variable_data, x = "Station", y = !!var_symbol, 
                      type = selectedStatApproach,
                      mean.ci = TRUE,
                      conf.level = as.integer(selectedConflevel),
                      violin.args = if(plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} 
                      else {list(trim=TRUE,alpha = 0.2)},
                      boxplot.args = if(plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} 
                      else {list(alpha = 0.2)},
                      pairwise.comparisons = TRUE, 
                      pairwise.annotation = TRUE,
                      pairwise.display = "s", 
                      sig.level = NA,
                      p.adjust.method = "fdr",
                      messages = FALSE,
                      title = title)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Extract stats and store in reactive variable
    test_stats <- extract_stats(p)
    
    # Extract Caption
    if (n_distinct(variable_data$Station) > 1){
      caption <- paste(test_stats$subtitle_data[["method"]][1], ":", test_stats$subtitle_data[["statistic"]][1], ", p-value:", test_stats$subtitle_data[["p.value"]][1])
      CDA_AS_caption_reactive$caption <- caption
    } else{CDA_AS_caption_reactive$caption <- NULL}
    
    # Compute distribution statistics by y_axis
    if (selectedStatApproach == "parametric"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "mean")[1])
    } else if (selectedStatApproach == "nonparametric"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "median")[1])
    }else if (selectedStatApproach == "robust"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "trimmed")[1])
    }else if (selectedStatApproach == "bayes"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "MAP")[1])
    }
    
    CDA_AS_caption_reactive$centrality_measure <- as.data.frame(distribution_stats)
    
    # Extract pairwise comparison
    if (n_distinct(variable_data$Station) > 2){
      # print(test_stats$pairwise_comparisons_data)
      CDA_AS_caption_reactive$pairwise_table <- as.data.frame(test_stats$pairwise_comparisons_data) %>% select(-expression)
    } else { CDA_AS_caption_reactive$pairwise_table <-NULL}
    
    list(variable_data = variable_data, selected_var = selected_var,time_resolution = time_resolution, title = title,
         selectedStatApproach=selectedStatApproach,selectedConflevel=selectedConflevel, plotType = plotType,
         p = p, CDA_AS_caption_reactive = CDA_AS_caption_reactive
    )
  })
  
  ## Output plot
  output$CDA_AS_plot <- renderPlotly({
    # Extract the result from the eventReactive object
    result <- CDA_AS_data_prep()
    p <-result$p
    return(p)
  })
  
  
  ## Output test statistics
  
  output$CDA_AS_Caption_Output <- renderText({
    # Extract the result from the eventReactive object
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if (!is.null(CDA_AS_caption_reactive$caption)) {
      CDA_AS_caption_reactive$caption
    }
  })
  
  output$CDA_AS_centrality_measure_title <- renderUI({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if (!is.null(CDA_AS_caption_reactive$centrality_measure)) {
      tags$h4("\nCentrality Measures")
    }
  })
  
  output$CDA_AS_centrality_measure_datatable <- renderTable({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if (!is.null(CDA_AS_caption_reactive$centrality_measure)) {
      CDA_AS_caption_reactive$centrality_measure
    }
  })
  
  output$CDA_AS_pairwise_comparison_title <- renderUI({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if(!is.null(CDA_AS_caption_reactive$pairwise_table)){
      tags$h4("\nPairwise Comparison")
    }
  })
  
  output$CDA_AS_pairwise_comparison_datatable <- renderTable({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <- result$CDA_AS_caption_reactive
    if(!is.null(CDA_AS_caption_reactive$pairwise_table)){
      CDA_AS_caption_reactive$pairwise_table
    }
  })
  
  output$CDA_AS_test_results <- renderUI({
    if (!is.null(CDA_AS_caption_reactive)) {
      tagList(
        textOutput("CDA_AS_Caption_Output"),
        uiOutput("CDA_AS_centrality_measure_title"),
        tableOutput("CDA_AS_centrality_measure_datatable"),
        uiOutput("CDA_AS_pairwise_comparison_title"),
        tableOutput("CDA_AS_pairwise_comparison_datatable")
      )
    }
  })
  
  ## Output insights
  AS_insightsText <- eventReactive(input$CDA_AS_Insights_button,{
    input$CDA_AS_Insights
  }, ignoreNULL = TRUE)
  output$CDA_AS_Insights_Output <- renderText({ AS_insightsText() })
  
  
  # CDA: Compare Across Time ----
  # 1. Dynamic UI
  output$CDA_AT_dynamic_time_resolution <- renderUI ({
    
    if (input$CDA_AT_time_resolution == "Years") {
      checkboxGroupInput("CDA_AT_selected_dates", label = "Select years", choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1])
    } else if (input$CDA_AT_time_resolution == "Months") {
      checkboxGroupInput("CDA_AT_selected_dates", label = "Select month", choices = unique(weather_data$Month_Name))      
    } else if (input$CDA_AT_time_resolution == "Months for a specified year") {
      list(selectInput("CDA_AT_selected_year", label = "Select year", choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1], multiple = FALSE),
           checkboxGroupInput("CDA_AT_selected_dates", label = "Select month", choices = unique(weather_data$Month_Name)) 
      )
    } else if (input$CDA_AT_time_resolution == "Months of different years") {
      list(checkboxGroupInput("CDA_AT_selected_dates", label = "Select year", choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1]),
           selectInput("CDA_AT_selected_month", label = "Select month", choices = unique(weather_data$Month_Name), multiple = FALSE) 
      )
    }
  })
  
  # 2. Normality Plot
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AT_checknormality_data_prep <- eventReactive(input$CDA_AT_checknormality_button, {
    req(length(input$CDA_AT_selected_dates) > 0)  # Ensure at least one "date" is selected
    
    selected_var <- input$CDA_AT_selected_var
    time_resolution <- input$CDA_AT_time_resolution
    selected_station <- input$CDA_AT_selected_station
    
    if (time_resolution == "Years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months for a specified year") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_year <- input$CDA_AT_selected_year
      
      variable_data <- weather_data %>%
        filter(Year %in% selected_year,
               Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months of different years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_month <- input$CDA_AT_selected_month
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Month_Name %in% selected_month,
               Station %in% selected_station)
    }
    
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {selected_var}
    time_title <- if(time_resolution == "Months") {"across months"} else if (time_resolution == "Years") {"across years"} 
    else if(time_resolution =="Months for a specified year") {paste("across months of", input$CDA_AT_selected_year)}
    else if (time_resolution == "Months of different years"){paste("for", input$CDA_AT_selected_month, "across years")}
    
    title <- paste("Distribution of", var_title, time_title, "for", selected_station)
    
    y_axis <- if(time_resolution == "Months") {"Month_Name"} else if (time_resolution == "Years") {"Year"} 
    else if(time_resolution =="Months for a specified year") {"Month_Name"}
    else if (time_resolution == "Months of different years"){"Year"}
    
    list(variable_data = variable_data, selected_var = selected_var, y_axis = y_axis, time_resolution = time_resolution, selected_station = selected_station, title = title)
  })
  
  ## Output normality plot
  output$CDA_AT_checknormality_plot <- renderPlot({
    
    result <- CDA_AT_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    time_resolution <- result$time_resolution
    title <- result$title
    y_axis <- result$y_axis
    
    
    ggplot(data = variable_data, aes(x = !!var_symbol, y = as.factor(!!rlang::sym(y_axis)))) +
      geom_density_ridges(fill =if (grepl("Rainfall", selected_var)) {"lightblue"} else if (grepl("Temperature", selected_var)) {"#FEC26B"}, alpha = 0.9) +
      labs(title = title) + 
      theme_ridges() +
      theme(legend.position = "none", axis.title.y = element_blank())  
  })
  
  ## Output table of normality test results 
  output$CDA_AT_checknormality_results <- renderTable({
    
    result <- CDA_AT_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    y_axis <- result$y_axis
    # time_resolution <- result$time_resolution
    
    ad_results <- variable_data %>%
      group_by(Group = as.factor(!!rlang::sym(y_axis))) %>%
      summarise(
        `Anderson-Darling Statistic` = list(ad.test(get(var_symbol))$statistic),
        `Anderson-Darling p.value` = list(ad.test(get(var_symbol))$p.value),
        `Shapiro-Wilk Statistic` = list(shapiro.test(get(var_symbol))$statistic),
        `Shapiro-Wilk p.value` = list(shapiro.test(get(var_symbol))$p.value)
      ) %>%
      unnest(cols = c(`Anderson-Darling Statistic`, `Anderson-Darling p.value`, `Shapiro-Wilk Statistic`, `Shapiro-Wilk p.value`)) %>%
      mutate(across(where(is.numeric), ~format(round(., 4), nsmall = 4))) %>%
      ungroup() %>%
      setNames(c(y_axis, names(.)[-1]))  # Dynamically rename the first column
    
    if (y_axis == "Month_Name") {
      ad_results <- ad_results %>% rename(Month = Month_Name)
    }
    
    ad_results
  })
  
  #3. Comparison Plot
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AT_caption_reactive <- reactiveValues() # Initializes a reactive value FOR CAPTION
  CDA_AT_data_prep <- eventReactive(input$CDA_AT_plot_button, {
    req(length(input$CDA_AT_selected_dates) > 0)  # Ensure at least one "date" is selected
    
    selected_var <- input$CDA_AT_selected_var
    time_resolution <- input$CDA_AT_time_resolution
    selected_station <- input$CDA_AT_selected_station
    title <- input$CDA_AT_plot_title # Take directly from input
    var_symbol <- rlang::sym(selected_var)
    selected_dates <- input$CDA_AT_selected_dates
    
    if (time_resolution == "Years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months for a specified year") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_year <- input$CDA_AT_selected_year
      
      variable_data <- weather_data %>%
        filter(Year %in% selected_year,
               Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months of different years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_month <- input$CDA_AT_selected_month
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Month_Name %in% selected_month,
               Station %in% selected_station)
    }
    
    y_axis <- if(time_resolution == "Months") {"Month_Name"} else if (time_resolution == "Years") {"Year"} 
    else if(time_resolution =="Months for a specified year") {"Month_Name"}
    else if (time_resolution == "Months of different years"){"Year"}
    
    selectedStatApproach<-input$CDA_AT_selectedStatApproach
    selectedConflevel<-input$CDA_AT_selectedConflevel
    plotType<-input$CDA_AT_plotType
    
    
    p <- ggbetweenstats(data = variable_data,
                        x = !!rlang::sym(y_axis),
                        y = !!var_symbol,
                        type = selectedStatApproach,
                        mean.ci = TRUE,
                        centrality.plotting = TRUE,
                        conf.level = as.integer(selectedConflevel),
                        violin.args = if(plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} else {list(trim=TRUE,alpha = 0.2)},
                        boxplot.args = if(plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} else {list(alpha = 0.2)},
                        pairwise.comparisons = TRUE,
                        pairwise.annotation = FALSE,
                        pairwise.display = "s",
                        sig.level = NA,
                        p.adjust.method = "fdr",
                        title = title,
                        messages = FALSE)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Extract stats and store in reactive variable
    test_stats <- extract_stats(p)
    
    # Extract Caption
    if (length(selected_dates) > 1){
      caption <- paste(test_stats$subtitle_data[["method"]][1], ":", test_stats$subtitle_data[["statistic"]][1], ", p-value:", test_stats$subtitle_data[["p.value"]][1])
      CDA_AT_caption_reactive$caption <- caption
    } else {      CDA_AT_caption_reactive$caption <- NULL}
    
    # Compute distribution statistics by y_axis
    if (selectedStatApproach == "parametric"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "mean")[1])
      CDA_AT_caption_reactive$centrality_measure <- as.data.frame(distribution_stats)
    } else if (selectedStatApproach == "nonparametric"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "median")[1])
    }else if (selectedStatApproach == "robust"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "trimmed")[1])
    }else if (selectedStatApproach == "bayes"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "MAP")[1])
    }
    CDA_AT_caption_reactive$centrality_measure <- as.data.frame(distribution_stats)
    
    
    # Extract pairwise comparison
    if (length(selected_dates) > 2){
      CDA_AT_caption_reactive$pairwise_table <- as.data.frame(test_stats$pairwise_comparisons_data) %>% select(-expression)
    } else { CDA_AT_caption_reactive$pairwise_table <-NULL}
    
    list(variable_data = variable_data, selected_var = selected_var, y_axis = y_axis, time_resolution = time_resolution, selected_station = selected_station, selected_dates = selected_dates, title = title,
         selectedStatApproach=selectedStatApproach,selectedConflevel=selectedConflevel, plotType = plotType, 
         p = p, CDA_AT_caption_reactive = CDA_AT_caption_reactive)
  })
  
  ## Output plot
  
  output$CDA_AT_plot <- renderPlotly({
    
    # Extract the result from the eventReactive object
    result <- CDA_AT_data_prep()
    p <-result$p
    
    return(p)
  })
  
  ## Output test statistics
  
  output$CDA_AT_Caption_Output <- renderText({
    # Extract the result from the eventReactive object
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if (!is.null(CDA_AT_caption_reactive$caption)) {
      CDA_AT_caption_reactive$caption
    }
  })
  
  output$CDA_AT_centrality_measure_title <- renderUI({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if (!is.null(CDA_AT_caption_reactive$centrality_measure)) {
      tags$h4("\nCentrality Measures")
    }
  })
  
  output$CDA_AT_centrality_measure_datatable <- renderTable({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if (!is.null(CDA_AT_caption_reactive$centrality_measure)) {
      CDA_AT_caption_reactive$centrality_measure
    }
  })
  
  output$CDA_AT_pairwise_comparison_title <- renderUI({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if(!is.null(CDA_AT_caption_reactive$pairwise_table)){
      tags$h4("\nPairwise Comparison")
    }
  })
  
  output$CDA_AT_pairwise_comparison_datatable <- renderTable({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <- result$CDA_AT_caption_reactive
    if(!is.null(CDA_AT_caption_reactive$pairwise_table)){
      CDA_AT_caption_reactive$pairwise_table
    }
  })
  
  output$CDA_AT_test_results <- renderUI({
    if (!is.null(CDA_AT_caption_reactive)) {
      tagList(
        textOutput("CDA_AT_Caption_Output"),
        uiOutput("CDA_AT_centrality_measure_title"),
        tableOutput("CDA_AT_centrality_measure_datatable"),
        uiOutput("CDA_AT_pairwise_comparison_title"),
        tableOutput("CDA_AT_pairwise_comparison_datatable")
      )
    }
  })
  
  
  ## Output insights
  AT_insightsText <- eventReactive(input$CDA_AT_Insights_button,{
    input$CDA_AT_Insights
  }, ignoreNULL = TRUE)
  output$CDA_AT_Insights_Output <- renderText({ AT_insightsText() })
  
  # Time Series: Exploratory Plots ----
  
  ## 1. Dynamic UI for ExploreTS
  
  ### Dynamic UI  For time resolution
  output$ExploreTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Week", "Month"))
    } else if (grepl("Temperature", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Day" ,"Week", "Month"))
    }
  })
  
  
  ### Update input start date and end date
  observe({
    start_date <- input$ExploreTS_startDate
    end_date <- input$ExploreTS_endDate
    # Calculate the minimum end date by adding one month to the start date
    min_end_date <- start_date %m+% months(1) # Adjust to get the day before a full month, ensuring a full month period
    
    # If the end date is less than the minimum end date, adjust it
    if (end_date < start_date || end_date < min_end_date) {
      # If the end date is selected before the start date, adjust the start date instead
      if (end_date < start_date) {
        # Ensure that adjusting the start date backward does not go before a minimum allowed date
        min_allowed_start_date <- as.Date("2021-01-01")
        new_start_date <- end_date %m-% months(1) + days(1) # Adjust to ensure a full month period from end date
        if (new_start_date < min_allowed_start_date) {
          new_start_date <- min_allowed_start_date
        }
        # Update the start date input to ensure a minimum 1 month period
        updateDateInput(session, "ExploreTS_startDate", value = new_start_date)
      } else {
        # Adjust the end date to ensure a minimum duration of 1 month
        if (min_end_date > as.Date("2023-12-31")) {
          # If the new minimum end date exceeds the maximum allowed date, adjust it to the maximum
          min_end_date <- as.Date("2023-12-31")
        }
        # Update the end date input to ensure a minimum 1 month period
        updateDateInput(session, "ExploreTS_endDate", value = min_end_date)
      }
    }
    
  })
  
  ### Instructions
  output$ExporeTS_Date_Instructions <- renderUI({
    HTML("<em>Please select a period of at least 1 month for plotting</em>")
  })
  ## 2. Prepare data and plot line graph
  ### Output Plot
  ExploreTS_timeSeriesPlot_DataTable_reactive <- reactiveVal()
  output$ExploreTS_timeSeriesPlot <- renderPlotly({
    
    req(length(input$ExploreTS_selectstation) > 0)  # Ensure stations are selected
    # Prepare Data
    selected_var <- input$ExploreTS_selected_var
    
    char_startDate <- as.character(input$ExploreTS_startDate)
    char_endDate <- as.character(input$ExploreTS_endDate)
    
    variable_data <- weather_tsbl %>%
      filter_index(char_startDate ~ char_endDate) %>%
      filter(Station %in% input$ExploreTS_selectstation)
    
    if (input$ExploreTS_time_resolution == "Day") {
      variable_data <- variable_data %>%
        mutate(ValueToPlot = .data[[selected_var]])
    } else if (input$ExploreTS_time_resolution == "Week") {
      variable_data <- variable_data %>%
        group_by_key() %>%
        index_by(year_week = ~ yearweek(.)) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    } else if (input$ExploreTS_time_resolution == "Month") {
      variable_data <- variable_data %>%
        as_tibble() %>%
        group_by(Station, Year, Month) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
    }
    
    # Store in reactive val for datatable
    ExploreTS_timeSeriesPlot_DataTable_reactive(variable_data)
    
    # Update plot annotations
    yaxis_title <- if(grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if(grepl("Temperature", selected_var )) {"Temperature (°C)"}
    time_title <- switch(input$ExploreTS_time_resolution,"Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {
      if(input$ExploreTS_time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
    title <- paste(time_title, var_title, "\n", char_startDate, "to", char_endDate)
    
    #Create plotly plot
    plot_ly(variable_data, x = ~Date, y = ~ValueToPlot, 
            type = 'scatter', mode = 'lines', 
            color = ~Station, hoverinfo = 'text',
            text = ~paste("<b>Station:</b>", Station,
                          "<br>", 
                          ifelse(input$ExploreTS_time_resolution == "Week", "<b>Week starting:</b> ", 
                                 ifelse(input$ExploreTS_time_resolution == "Month", "<b>Month:</b> ", "<b>Date:</b> ")), 
                          ifelse(input$ExploreTS_time_resolution == "Month", format(Date, "%Y %b"), as.character(Date)),
                          "<br><b>", ifelse(grepl("Rainfall", selected_var), "Total Rainfall (mm)", selected_var), ":</b> ", ValueToPlot)) %>%
      layout(title = title,
             xaxis = list(title = "", 
                          range = c(char_startDate, char_endDate), 
                          rangeslider = list(type = "date", range = c(char_startDate, char_endDate))),
             yaxis = list(title = yaxis_title))
  })
  
  ### Output datatable
  output$ExploreTS_timeSeriesPlot_DataTable <- DT::renderDataTable({
    req(length(input$ExploreTS_selectstation) > 0)
    
    
    data_frame <- as.data.frame(ExploreTS_timeSeriesPlot_DataTable_reactive())
    print(data_frame)
    
    data_frame$Station <-  as.factor(data_frame$Station)
    
    if (input$ExploreTS_time_resolution == "Week") {
      data_frame <- data_frame %>% select(Station, Date, year_week, ValueToPlot)
      data_frame$year_week <- as.character(data_frame$year_week)
      data_frame <- data_frame %>%
        rename(Week := year_week)
    } else if (input$ExploreTS_time_resolution == "Month"){
      data_frame <- data_frame %>% select(Station, Date, Month, ValueToPlot)
      data_frame <- data_frame %>%
        mutate(Month = month.name[Month])
    } else if (input$ExploreTS_time_resolution == "Day") {
      data_frame <- data_frame %>% select(Station, Date, ValueToPlot)
      
    }
    
    data_frame <- data_frame %>%
      rename(!!input$ExploreTS_selected_var := ValueToPlot)
    # data_frame <- data_frame %>%
    #   rename_with(~"ValueToPlot", .cols = all_of(input$ExploreTS_selected_var))
    
    datatable(data_frame, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              filter = 'top',
              options = list(pageLength = 10, scrollX = TRUE))
    
  })
  
  ## 3. Plot horizon plot 
  output$ExploreTS_HoriPlot <- renderPlot({
    req(length(input$ExploreTS_selectstation) > 0)
    # Prepare Data
    selected_var <- input$ExploreTS_selected_var
    
    char_startDate <- as.character(input$ExploreTS_startDate)
    char_endDate <- as.character(input$ExploreTS_endDate)
    
    variable_data <- weather_tsbl %>%
      filter_index(char_startDate ~ char_endDate) %>%
      filter(Station %in% input$ExploreTS_selectstation)
    
    if (input$ExploreTS_time_resolution == "Day") {
      variable_data <- variable_data %>%
        mutate(ValueToPlot = .data[[selected_var]])
    } else if (input$ExploreTS_time_resolution == "Week") {
      variable_data <- variable_data %>%
        group_by_key() %>%
        index_by(year_week = ~ yearweek(.)) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    } else if (input$ExploreTS_time_resolution == "Month") {
      variable_data <- variable_data %>%
        as_tibble() %>%
        group_by(Station, Year, Month) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
    }
    
    # Store in reactive val for datatable
    ExploreTS_timeSeriesPlot_DataTable_reactive(variable_data)
    
    # Update plot annotations
    yaxis_title <- if(grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if(grepl("Temperature", selected_var )) {"Temperature (°C)"}
    time_title <- switch(input$ExploreTS_time_resolution,"Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {
      if(input$ExploreTS_time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
    title <- paste(time_title, var_title, "\n", char_startDate, "to", char_endDate)
    
    # Compute origin and  horizon scale cutpoints: 
    cutpoints <- variable_data %>% 
      mutate(
        outlier = between(
          .data[["ValueToPlot"]], 
          quantile(.data[["ValueToPlot"]], 0.25, na.rm = TRUE) -
            1.5 * IQR(.data[["ValueToPlot"]], na.rm = TRUE),
          quantile(.data[["ValueToPlot"]], 0.75, na.rm = TRUE) +
            1.5 * IQR(.data[["ValueToPlot"]], na.rm = TRUE))) %>% 
      filter(outlier)
    
    
    ori <- sum(range(cutpoints[["ValueToPlot"]]))/2
    sca <- seq(range(cutpoints[["ValueToPlot"]])[1], range(cutpoints[["ValueToPlot"]])[2], length.out = 7)[-4]
    
    
    ori <- round(ori, 2) # The origin, rounded to 2 decimal places
    sca <- round(sca, 2) # The horizon scale cutpoints
    
    # reverse_T <- ifelse(grepl("Rainfall", selected_var), FALSE, TRUE)
    palette <- if (grepl("Rainfall", selected_var)) {"Blues"} else if (grepl("Temperature", selected_var)) {"YlOrRd"}
    # Plot horizon plot
    variable_data %>% ggplot() +
      geom_horizon(aes(x = Date, 
                       y = .data[["ValueToPlot"]],
                       fill = after_stat(Cutpoints)), 
                   origin = ori, horizonscale = sca) +
      scale_fill_hcl(palette = palette, reverse = T) +
      facet_grid(Station ~ .)+
      theme_few() +
      theme(
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()
      ) +
      scale_x_date(expand = c(0, 0), 
                   date_breaks = "1 month", 
                   date_labels = "%b") +
      xlab('Date') +
      ggtitle(title)
    
    
  })
  
  
  # Time Series: Decomposition ----
  
  ## 1.1 Dynamic UI for DecompTS_dynamic_time_resolution
  output$DecompTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$DecompTS_selected_var)) {
      radioButtons("DecompTS_time_resolution", label = "Select time resolution", c("Week"))
    } else if (grepl("Temperature", input$DecompTS_selected_var)) {
      radioButtons("DecompTS_time_resolution", label = "Select time resolution", c("Day" ,"Week"))
    }
  })
  
  output$DecompTS_startDate_ui <- renderUI({
    if(input$DecompTS_time_resolution == "Day"){
      dateInput("DecompTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-11-30")
    } else if (input$DecompTS_time_resolution == "Week"){
      dateInput("DecompTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-06-25")
    }
    
    
  })
  
  ### Instructions
  output$DecompTS_Date_Instructions <- renderUI({
    
    if(input$DecompTS_time_resolution == "Day"){
      HTML("<em>Please select a period of at least 30 days for plotting. </em>")
    } else if (input$DecompTS_time_resolution == "Week"){
      HTML("<em>Please select a period of at least 26 weeks for plotting. </em>")    }
    
  })
  
  ## 1.2 Dynamic UI for DecompTS_dynamiclags
  output$DecompTS_dynamiclags <- renderUI ({
    if (input$DecompTS_time_resolution == "Day") {
      sliderInput("DecompTS_lags", "Number of Lags", min = 1, max = 365, value = 20, step = 1)
    } else if (input$DecompTS_time_resolution == "Week") {
      sliderInput("DecompTS_lags", "Number of Lags", min = 1, max = 52, value = 20, step = 1)
    }
  })
  ## 1.3 Dynamic UI for DecompTS_dynamic_autoSTL
  output$DecompTS_dynamic_autoSTL <- renderUI({
    if (input$DecompTS_chooseautoSTL == "No") {
      if (input$DecompTS_time_resolution == "Day") {
        list(sliderInput("DecompTS_TrendWindow", "Trend Window", min = 1, max = 365, value = 20, step = 1),
             sliderInput("DecompTS_SeasonWindow", "Season Window", min = 1, max = 365, value = 20, step = 1))
      } else if (input$DecompTS_time_resolution == "Week") {
        list(sliderInput("DecompTS_TrendWindow", "Trend Window", min = 1, max = 52, value = 20, step = 1),
             sliderInput("DecompTS_SeasonWindow", "Season Window", min = 1, max = 52, value = 20, step = 1))
      }
    }
  })
  
  
  ## 2. ACF Plot and PACF Plot
  ### ACF plot
  output$DecompTS_ACFPlot <- renderPlotly({
    
    result <- TS_prepareVariableData(input$DecompTS_selected_var,as.character(input$DecompTS_startDate),"2023-12-31",input$DecompTS_selected_station,input$DecompTS_time_resolution,weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    char_startDate <- result$char_startDate
    char_endDate <- result$char_endDate
    
    ACF <- variable_data %>%
      ACF(ValueToPlot, lag_max = input$DecompTS_lags) %>%
      autoplot() +
      labs(title = paste("ACF plot of", title,  "\n", char_startDate, "to", char_endDate)) +
      theme_minimal()
    
    ggplotly(ACF)
  })
  ### PACF plot
  output$DecompTS_PACFPlot <- renderPlotly({ 
    result <- TS_prepareVariableData(input$DecompTS_selected_var,as.character(input$DecompTS_startDate),"2023-12-31",input$DecompTS_selected_station, input$DecompTS_time_resolution,weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    char_startDate <- result$char_startDate
    char_endDate <- result$char_endDate
    
    PACF <- variable_data %>%
      PACF(ValueToPlot, lag_max = input$DecompTS_lags) %>%
      autoplot() +
      labs(title = paste("PACF plot of", title,  "\n", char_startDate, "to", char_endDate)) +
      theme_minimal()
    
    ggplotly(PACF)
  })
  
  # 3. DecompTS_STLPlot
  output$DecompTS_STLPlot <- renderPlotly({
    result <- TS_prepareVariableData(input$DecompTS_selected_var,as.character(input$DecompTS_startDate),"2023-12-31", input$DecompTS_selected_station,input$DecompTS_time_resolution,weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    char_startDate <- result$char_startDate
    char_endDate <- result$char_endDate
    
    if(input$DecompTS_chooseautoSTL == "No") {
      trend_window <- input$DecompTS_TrendWindow
      season_window <- input$DecompTS_SeasonWindow
    } else { 
      trend_window <- NULL
      season_window <- NULL
    }
    
    # Note that variable name is always 'ValueToPlot', because of the reactive variable. So, use it directly in STL
    stl_fit <- variable_data %>%
      model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window))) %>%
      components()
    
    # Convert to tibble and perform common transformations
    stl_fit_tibble <- as_tibble(stl_fit) %>%
      mutate(trend = round(trend, 2),
             season_adjust = round(season_adjust, 2),
             season_year = round(season_year, 4),
             remainder = round(remainder, 4))
    
    # Conditionally mutate season_week if the resolution is "Day"
    if(input$DecompTS_time_resolution == "Day") {
      stl_fit_tibble <- stl_fit_tibble %>%
        mutate(season_week = round(season_week, 4))
    }
    
    # Visualize the STL components
    plot_stl <- stl_fit %>%
      autoplot() +
      labs(title = paste(title, "\n", char_startDate, "to", char_endDate))
    
    # Convert the ggplot object to a plotly object
    ggplotly(plot_stl) %>%
      layout(plot_bgcolor="#edf2f7")
  })
  # Time Series: Forecasting ----
  
  ## 1.1 Dynamic UI for ForecastTS_dynamic_time_resolution
  output$ForecastTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ForecastTS_selected_var)) {
      radioButtons("ForecastTS_time_resolution", label = "Select time resolution", c("Week"))
    } else if (grepl("Temperature", input$ForecastTS_selected_var)) {
      radioButtons("ForecastTS_time_resolution", label = "Select time resolution", c("Day" ,"Week"))
    }
  })
  
  output$ForecastTS_startDate_ui <- renderUI({
    if(input$ForecastTS_time_resolution == "Day"){
      dateInput("ForecastTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-11-30")
    } else if (input$ForecastTS_time_resolution == "Week"){
      dateInput("ForecastTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-06-25")
    }
    
  })
  ### Instructions
  output$ForecastTS_Date_Instructions <- renderUI({
    
    if(input$ForecastTS_time_resolution == "Day"){
      HTML("<em>Please select a period of at least 30 days for plotting. </em>")
    } else if (input$ForecastTS_time_resolution == "Week"){
      HTML("<em>Please select a period of at least 26 weeks for plotting. </em>")    }
    
  })
  # observe({
  #   # This will now only react to changes in 'input$ForecastTS_dynamic_time_resolution'
  #   if(input$ForecastTS_dynamic_time_resolution == "Week"){
  #     updateDateInput(session, "ForecastTS_startDate", max = "2023-06-01")
  #   }
  # })
  
  ## 1.3 Dynamic UI for ForecastTS_dynamic_chooseautoSTL
  output$ForecastTS_dynamic_chooseautoSTL <- renderUI({
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))
    ) {radioButtons("ForecastTS_chooseautoSTL", label = "Use Auto STL?" ,c("Yes", "No"))
    }
  })
  
  ## 1.3 Dynamic UI for ForecastTS_dynamic_model_parameters
  output$ForecastTS_dynamic_model_parameters <- renderUI({
    # Check if any of the STL options are selected
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))) {
      if(input$ForecastTS_chooseautoSTL == "No"
      ){
        list(sliderInput("ForecastTS_TrendWindow", "Trend Window", min = 1, max = 365, value = 20),
             sliderInput("ForecastTS_SeasonWindow", "Season Window", min = 1, max = 365, value = 20))
      } 
    }
  })
  ## 1.4 Dynamic UI for ForecastTS_dynamic_forecast_period
  output$ForecastTS_dynamic_forecast_period <- renderUI({
    end_date <- as.Date("2023-12-31")
    
    if (input$ForecastTS_time_resolution == "Day"){
      total_days <- as.numeric(difftime(end_date, input$ForecastTS_startDate, units = "days"))
      # Calculate maximum future forecasting horizon
      max_forecast_period <- floor(total_days / 12)
      sliderInput("ForecastTS_forecast_period", "Select Forecast Period (days)", min = 1, max = max(max_forecast_period, 1), value = min(10, max_forecast_period), step = 1)
    } else if (input$ForecastTS_time_resolution == "Week"){
      total_weeks <- as.numeric(difftime(end_date, input$ForecastTS_startDate, units = "weeks"))
      max_forecast_period <- floor(total_weeks / 12)
      sliderInput("ForecastTS_forecast_period", "Select Forecast Period (weeks)", min = 1, max = max(max_forecast_period, 1), value = min(10, max_forecast_period), step = 1)
    }
    
  })
  
  
  ## 2. Prepare data and create eventReactive object that updates only when the Build Model button is clicked
  model_building <- eventReactive(input$ForecastTS_build_model, {
    req(length(input$ForecastTS_selected_models) > 0)  # Ensure models are selected
    
    # Proceed with model fitting and forecasting logic
    result <- TS_prepareVariableData(input$ForecastTS_selected_var,as.character(input$ForecastTS_startDate),"2023-12-31", input$ForecastTS_selected_station,input$ForecastTS_time_resolution, weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    
    split_point <- nrow(variable_data) * input$ForecastTS_train_test_split
    train_data <- variable_data %>% slice(1:floor(split_point))
    test_data <- variable_data %>% slice((floor(split_point) + 1):n())
    
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))){
      if(input$ForecastTS_chooseautoSTL == "No") {
        trend_window <- input$ForecastTS_TrendWindow
        season_window <- input$ForecastTS_SeasonWindow
      } else { 
        trend_window <- NULL
        season_window <- NULL
      }
    }
    
    ForecastTS_model_list <- list(
      STL_Naive = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), NAIVE(season_adjust)),
      STL_ARIMA = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ARIMA(season_adjust)),
      STL_ETS = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ETS(season_adjust ~ season("N"))),
      AUTO_ARIMA = ARIMA(ValueToPlot),
      AUTO_Prophet = prophet(ValueToPlot),
      AUTO_ETS = ETS(ValueToPlot)
    )
    
    
    selected_models <- ForecastTS_model_list[names(ForecastTS_model_list) %in% input$ForecastTS_selected_models]
    train_fit <- model(train_data, !!!selected_models) # Fit models to the train data
    forecast_horizon <- nrow(test_data) # Set forecasting horizon to length of test data
    forecasts <- forecast(train_fit, h = forecast_horizon) 
    
    selected_var <- input$ForecastTS_selected_var
    var_title <- if (grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {"Temperature (°C)"}
    
    # Return data to be used for plotting for Forecast validation
    list(variable_data = variable_data, train_fit = train_fit, forecasts = forecasts, test_data = test_data, train_data = train_data, title = title, var_title = var_title, selected_var = selected_var)
  })
  
  ## 3. Model Calibration tab
  ### Forecast Validation Plot
  output$ForecastTS_forecast_validation_plot <- renderPlotly({
    # Extract the result from the eventReactive object
    result <- model_building()
    # Extract data and variables from reactive expression
    variable_data <- result$variable_data
    title <- result$title
    train_data <- result$train_data
    test_data <- result$test_data
    forecasts <- result$forecasts
    selected_var <-result$selected_var
    var_title <- result$var_title
    
    plot <- autoplot(variable_data, ValueToPlot) + 
      autolayer(forecasts, level = NULL) + 
      labs(title = paste("Forecast Validation", title),  y = var_title) + 
      theme_minimal()
    
    p <- ggplotly(plot,
                  tooltip = c("x", "y", ".model"))
    
    
    return(p)
    
  })
  ### Residual Plot
  output$ForecastTS_residual_plot <- renderPlotly({
    # Extract the result from the eventReactive object
    result <- model_building()
    # Extract data and variables from reactive expression
    variable_data <- result$variable_data
    train_fit <- result$train_fit
    
    residual <- train_fit %>% augment()
    
    a <- autoplot(residual, .innov) +
      labs(title = "Residual Plot") +
      theme_minimal() 
    
    a_plotly <- ggplotly(a) %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.4, xanchor = "center", yanchor = "top"),
             margin = list(b = 80))
    return(a_plotly)
  })
  
  ### Data Table
  output$ForecastTS_buildModel_DataTable <- DT::renderDataTable({
    result <- model_building()
    test_data <- result$test_data
    forecasts <- result$forecasts
    
    accuracy_metrics <- accuracy(forecasts, test_data) %>%
      arrange(.model) %>%
      select(.model, .type, RMSE, MAE, MAPE) %>%
      mutate(across(c(RMSE, MAE, MAPE), round, 2))
    
    datatable(accuracy_metrics, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              options = list(pageLength = 10,scrollX=T))
    
  })
  
  ## 4. Prepare data and create eventReactive object that updates only when the Forecast button is clicked
  future_forecast <- eventReactive(input$ForecastTS_future_forecast, {
    req(length(input$ForecastTS_selected_models) > 0)
    
    
    # Proceed with model fitting and forecasting logic
    result <- TS_prepareVariableData(input$ForecastTS_selected_var,as.character(input$ForecastTS_startDate),"2023-12-31", input$ForecastTS_selected_station,input$ForecastTS_time_resolution, weather_tsbl)
    variable_data <- result$variable_data
    title <- result$title
    selected_var <- input$ForecastTS_selected_var
    var_title <- if (grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {"Temperature (°C)"}
    
    
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))){
      if(input$ForecastTS_chooseautoSTL == "No") {
        trend_window <- input$ForecastTS_TrendWindow
        season_window <- input$ForecastTS_SeasonWindow
      } else { 
        trend_window <- NULL
        season_window <- NULL
      }
    }
    ForecastTS_model_list <- list(
      STL_Naive = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), NAIVE(season_adjust)),
      STL_ARIMA = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ARIMA(season_adjust)),
      STL_ETS = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ETS(season_adjust ~ season("N"))),
      AUTO_ARIMA = ARIMA(ValueToPlot),
      AUTO_Prophet = prophet(ValueToPlot),
      AUTO_ETS = ETS(ValueToPlot)
    )
    
    selected_models <- ForecastTS_model_list[names(ForecastTS_model_list) %in% input$ForecastTS_selected_models]
    full_fit <- model(variable_data, !!!selected_models) # Fit models to the full dataset 
    future_horizon <- input$ForecastTS_forecast_period
    full_forecast <- forecast(full_fit, h = future_horizon)
    
    full_forecast_df <- as_tibble(full_forecast)
    
    full_forecast_df <- full_forecast_df %>%
      mutate(.mean = round(.mean, 2)) %>%
      mutate(n=n(), sd=sd(.mean)) %>%
      mutate(se=sd/sqrt(n-1)) %>%
      mutate(Lower = round(.mean - (1.96 * se), 2),
             Upper = round(.mean + (1.96 * se), 2)) 
    
    if (input$ForecastTS_time_resolution == "Week"){
      full_forecast_df <- full_forecast_df  %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    }
    
    # Initialize an empty plotly object
    p <- plot_ly()
    
    # Unique models for iteration and color assignment
    unique_models <- unique(full_forecast_df$.model)
    colors <- RColorBrewer::brewer.pal(n = length(unique_models), name = "Set1")
    
    # Loop through each model to add to the plot
    for (i in seq_along(unique_models)) {
      model_name <- unique_models[i]
      
      # Filter data for the current model
      model_data <- filter(full_forecast_df, .model == model_name)
      
      # Define the custom hovertemplate for lines
      if (input$ForecastTS_time_resolution == "Day"){
        hovertemplate_line <- paste(
          "Date: %{x}<br>",
          var_title, ": %{y:.2f}<br>",
          "Model: ", model_name, "<br>",
          "95% CI: [%{customdata[0]:.2f}, %{customdata[1]:.2f}]<extra></extra>"
        )
      } else if(input$ForecastTS_time_resolution == "Week") {
        hovertemplate_line <- paste(
          "Week starting: %{x}<br>",
          var_title, ": %{y:.2f}<br>",
          "Model: ", model_name, "<br>",
          "95% CI: [%{customdata[0]:.2f}, %{customdata[1]:.2f}]<extra></extra>"
        )
      }
      
      # Custom data for the line (lower and upper CI values)
      custom_data <- mapply(function(lower, upper) list(lower, upper), model_data$Lower, model_data$Upper, SIMPLIFY = FALSE)
      
      # Add forecast line with custom tooltip and legend grouping
      p <- add_lines(p, data = model_data, x = ~Date, y = ~`.mean`, name = model_name,
                     line = list(color = colors[i]), hovertemplate = hovertemplate_line,
                     customdata = custom_data, legendgroup = model_name)
      
      # Add confidence interval ribbon with legend grouping, but no separate legend entry
      p <- add_ribbons(p, data = model_data, x = ~Date, ymin = ~Lower, ymax = ~Upper,
                       fillcolor = scales::alpha(colors[i], 0.2), line = list(color = "transparent"),
                       legendgroup = model_name, showlegend = FALSE, hoverinfo = "skip")
    }
    
    
    
    # Customize layout
    p <- p %>% layout(title = paste("Future Forecast Plot with 95% CI for", title),
                      xaxis = list(title = "Date"),
                      yaxis = list(title = var_title),
                      legend = list(title = list(text = 'Model')),
                      hovermode = 'closest')
    
    
    # Return data to be used for plotting for Future Forecast
    list(p = p, full_forecast_df= full_forecast_df)
    
  })
  
  ## 5. Forecast Result tab 
  
  ### Future Forecast Plot
  output$ForecastTS_future_forecast_plot <- renderPlotly({
    
    result <- future_forecast()
    p<-result$p
    p
    
    
  })
  
  ### Data Table
  output$ForecastTS_future_forecast_DataTable <- DT::renderDataTable({
    result <- future_forecast()
    
    full_forecast_df <- result$full_forecast_df
    
    col<- full_forecast_df %>% 
      select(.model, Date, .mean) %>% 
      rename(`Forecasted Value` = .mean) %>%
      mutate(`Forecasted Value` = round(`Forecasted Value`, 2),
             .model = as.factor(.model))
    
    datatable(col, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              filter = 'top',
              options = list(pageLength = 10,scrollX=T))
  })
  
  
  # Geospatial: Map ----
  ## 1. Dynamic UI: selected time_resolution
  output$GS_dynamic_time_resolution <- renderUI({
    if (input$GS_time_resolution == "Day") {
      airDatepickerInput("GS_selected_date", label = "Select date", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM-dd")
    } else if (input$GS_time_resolution == "Month") {
      airMonthpickerInput("GS_selected_date", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$GS_time_resolution == "Year") {
      airYearpickerInput("GS_selected_date", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  ## 2.eventReactive
  GS_tmap_event <- eventReactive(input$GS_updatetmap, {
    
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    main_title <- paste(results$main_title, "across 11 stations in Singapore")
    
    list(variable_data = results$variable_data, variable_data_sf = results$variable_data_sf, legend_title= results$legend_title, main_title = main_title, palette= results$palette, var_title = results$var_title)
  })
  
  ## 3. Output plot
  output$GS_tmap <- renderTmap({
    
    GS_reactiveDataTmap <- GS_tmap_event()
    
    dynamicPopupVars <- setNames(list(GS_reactiveDataTmap$legend_title), GS_reactiveDataTmap$legend_title)
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    tm <- tm_shape(mpsz2019) +
      tm_borders() +
      tm_shape(GS_reactiveDataTmap$variable_data_sf) +
      tm_dots(col = 'ValueToPlot', popup.vars = dynamicPopupVars,palette = GS_reactiveDataTmap$palette, size = 0.5, scale = 0.5, title = GS_reactiveDataTmap$legend_title)  +
      tm_layout(title = GS_reactiveDataTmap$main_title) +
      tm_view(set.view = c(lon = 103.8198, lat = 1.3521, zoom = 11)) # Centered on Singapore with an appropriate zoom level
    
    tm
  })
  ### Plot title
  output$GS_tmap_title <- renderUI({
    title_text <- GS_tmap_event()$main_title
    tags$h4(title_text)
  })
  
  ### Data Table
  output$GS_tmap_DataTable <- DT::renderDataTable({ 
    # DT::datatable(head(mtcars), options = list(pageLength = 5))
    GS_reactiveDataTmap <- GS_tmap_event()
    
    variable_data <- GS_reactiveDataTmap$variable_data %>% select(-ValueToPlot)
    
    variable_data$Station <-  as.factor(variable_data$Station)
    
    datatable(variable_data, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              filter = 'top',
              options = list(pageLength = 10,scrollX=T))
  })
  
  
  
  # Geospatial: IDW ----
  
  ## 1. Dynamic UI: IDW Parameters
  ### Removed
  
  ## 2. eventReactive
  GS_IDW_event <- eventReactive(input$GS_updateIDW, {
    
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    raster_layer <- GS_rasterlayer(input$GS_IDW_res)
    variable_data <-results$variable_data
    variable_data_sf <- results$variable_data_sf
    main_title <- paste(results$main_title, "in Singapore")
    legend_title <- results$legend_title
    palette<- results$palette
    grid <- raster_layer$grid
    coop <- raster_layer$coop
    
    # Set nmax value
    nmax = input$GS_IDW_nmax
    
    # Create gstat object
    res <- gstat(formula = ValueToPlot ~ 1,locations = variable_data_sf, set = list(idp = 0), nmax = nmax)
    # Predict values
    resp <- predict(res,coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    pred <- rasterize(resp, grid, field="pred", fun="mean")
    
    list(variable_data = results$variable_data, variable_data_sf = results$variable_data_sf, legend_title= results$legend_title, main_title = main_title, palette= results$palette, var_title = results$var_title, pred = pred)
  })
  
  
  # 3. Output plot
  output$GS_IDW_map <- renderPlot({
    
    GS_reactiveDataIDW <- GS_IDW_event()
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataIDW$pred) +
      tm_raster(title = GS_reactiveDataIDW$legend_title, alpha = 0.6, palette = GS_reactiveDataIDW$palette) +
      tm_layout(main.title = GS_reactiveDataIDW$main_title, main.title.position = "center", main.title.size = 1.2, legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
  })
  
  # Geospatial: OK ----
  ## 1. Dynamic UI: OK Parameters
  
  output$GS_OK_range_reactiveUI <-  renderUI({
    if (input$GS_OK_model == "Pow") {
      sliderInput("GS_OK_range", "range", min = 0, max = 2, value = 1, step = 0.5)
    } else
    {sliderInput("GS_OK_range", "range", min = 2000, max = 10000, value = 5000, step = 200)}
  })
  
  
  ## 2. eventReactive
  GS_OK_event <- eventReactive(input$GS_updateOK, {
    
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    raster_layer <- GS_rasterlayer(input$GS_OK_res)
    variable_data <-results$variable_data
    variable_data_sf <- results$variable_data_sf
    main_title <- paste(results$main_title, "in Singapore")
    legend_title <- results$legend_title
    palette<- results$palette
    
    grid <- raster_layer$grid
    coop <- raster_layer$coop
    
    # Generate Experimental Variogram
    v <- variogram(ValueToPlot ~ 1,
                   data = variable_data_sf)
    
    # Fit Variogram
    fv <- fit.variogram(object = v,
                        model = vgm(psill = input$GS_OK_psill, model = input$GS_OK_model, range = input$GS_OK_range, nugget = input$GS_OK_nugget))
    
    # perform spatial interpolation
    k <- gstat(formula = ValueToPlot ~ 1,
               data = variable_data_sf,
               model = fv)
    
    # estimate the unknown grids
    resp <- predict(k,coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    
    # create a raster surface data object
    kpred <- rasterize(resp, grid,
                       field = "pred")
    
    # Extracting the variance
    resp$variance <- resp$var1.var
    
    # Create a raster surface data object for variance
    kvar <- rasterize(resp, grid, field = "variance")
    
    list(variable_data = results$variable_data, variable_data_sf = results$variable_data_sf, legend_title= results$legend_title, main_title = main_title, palette= results$palette, var_title = results$var_title, 
         kpred = kpred, kvar = kvar, v = v, fv = fv, UI_title = "Result of Interpolation")
  })
  
  # 3. Output plots
  output$GS_OK_variogram <- renderPlot({
    
    GS_reactiveDataOK <- GS_OK_event()
    
    plot(GS_reactiveDataOK$v, cex = 1.5, main = "Experimental Variogram")
    
  })
  output$GS_OK_fitted_variogram <- renderPlot({
    
    GS_reactiveDataOK <- GS_OK_event()
    
    plot(GS_reactiveDataOK$v, GS_reactiveDataOK$fv, cex = 1.5, main = "Fitted Variogram")
    
  })
  output$GS_OK_map <- renderPlot({
    GS_reactiveDataOK <- GS_OK_event()
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataOK$kpred) + 
      tm_raster(title = GS_reactiveDataOK$legend_title, alpha = 0.6, palette = GS_reactiveDataOK$palette) +
      tm_layout(main.title = GS_reactiveDataOK$main_title, main.title.position = "center", main.title.size = 1.2, 
                legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
  })
  output$GS_OK_prediction_variance <- renderPlot({
    GS_reactiveDataOK <- GS_OK_event()
    variance_title <- "Prediction Variance"
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataOK$kvar) + 
      tm_raster(title = variance_title, alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = "Kriging Prediction Variance", main.title.position = "center", main.title.size = 1.2, legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
    
  })
  
  
  output$GS_OK_result_title <- renderUI({
    UI_title <- GS_OK_event()$UI_title
    tags$h4(UI_title)
  })
}
# Section 9: Run the application ---- 
shinyApp(ui, server)# Section 1: Set up ----
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, ggthemes, plotly, sf, terra, viridis, ggHoriPlot, ggstatsplot, rstantools, ISOweek, DT, nortest, ggridges, tsibble, tsibbledata, feasts, fable, fabletools, fable.prophet, RColorBrewer, urca, bslib, parameters, shinybusy)

# Section 1.1: Variables and Functions ----
## Import data 

# Load weather_data_cleaned.csv
weather_data <- read_csv("data/weather_data_cleaned.csv") %>%
  mutate(
    Date = as.Date(Date),
    Station = as.factor(Station),
    Latitude = Latitude,
    Longitude = Longitude
  )

# Define variables for selection
variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")
variables_select <- c(
  "Total Rainfall (mm)" = "Daily Rainfall Total (mm)",
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
  "Maximum Temperature (°C)" = "Maximum Temperature (°C)"
)

# Convert to tsibble
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)

# Station list
Station <- weather_tsbl %>% distinct(factor(Station))

# Forecast model choices
ForecastTS_model_choices <- list(
  "STL Naive" = "STL_Naive",
  "STL ARIMA" = "STL_ARIMA",
  "STL ETS" = "STL_ETS",
  "AUTO ARIMA" = "AUTO_ARIMA",
  "AUTO Prophet" = "AUTO_Prophet",
  "AUTO ETS" = "AUTO_ETS"
)

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")
variables_select <- c("Total Rainfall (mm)" = "Daily Rainfall Total (mm)", "Mean Temperature (°C)" = "Mean Temperature (°C)", "Minimum Temperature (°C)" = "Minimum Temperature (°C)" , "Maximum Temperature (°C)" = "Maximum Temperature (°C)")

## Used for Time Series module
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)
Station <- weather_tsbl %>% distinct(factor(Station))
ForecastTS_model_choices <- list("STL Naive" = "STL_Naive","STL ARIMA" = "STL_ARIMA","STL ETS" = "STL_ETS","AUTO ARIMA" = "AUTO_ARIMA","AUTO Prophet" = "AUTO_Prophet","AUTO ETS" = "AUTO_ETS")

## Used for Spatial Interpolation module
GS_model_choices <- c("Nug", "Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Pen","Per","Wav","Hol","Log","Pow","Spl")
# Create polygon boundary from station locations
station_points <- weather_data %>%
  select(Station, Latitude, Longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 3414)

# Create convex hull around station points as substitute for boundary
mpsz2019 <- st_convex_hull(st_union(station_points))
mpsz2019 <- st_sf(geometry = mpsz2019)

## Function to calculate ValueToPlot based on  selected variable.
## Used in Time Series and Spatial Interpolation modules
calculateValueToPlot <- function(data, var) {
  if (grepl("Rainfall", var)) {round(sum(data[[var]], na.rm = TRUE),2)} else if (grepl("Temperature", var)) {round(mean(data[[var]], na.rm = TRUE), 2)}
}

## Function to prepare data for CDA Across Stations tab
CDA_AS_prepareVariableData <- function(selected_var, time_resolution, selected_date, selected_stations, weather_data){
  if (time_resolution == "Month") {
    selected_month <- format(as.Date(selected_date), "%m")
    selected_year <- format(as.Date(selected_date), "%Y")
    variable_data <- weather_data %>% filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month), Station %in% selected_stations)
  } else if (time_resolution == "Year") {
    selected_year <- as.character(format(selected_date, "%Y"))
    variable_data <- weather_data %>% filter(format(Date, "%Y") == selected_year, Station %in% selected_stations)
  } 
  return(list(selected_var = selected_var, variable_data = variable_data, time_resolution = time_resolution, selected_date = selected_date, selected_stations = selected_stations))
}

## Function to prepare data for TS module
TS_prepareVariableData <- function(selected_var, char_startDate, char_endDate, selected_station, time_resolution, weather_tsbl) {
  variable_data <- weather_tsbl %>%
    filter_index(char_startDate ~ char_endDate) %>%
    filter(Station == selected_station)
  if (time_resolution == "Day") {
    variable_data <- variable_data %>% mutate(ValueToPlot = .data[[selected_var]])
  } else if (time_resolution == "Week") {
    variable_data <- variable_data %>%
      group_by_key() %>%
      index_by(year_week = ~ yearweek(.)) %>%
      summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
      mutate(Date = floor_date(as.Date(year_week), unit = "week"))
  }
  time_title <- switch(time_resolution, "Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
  var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {if(time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
  title <- paste(time_title, var_title, "for", selected_station)
  return(list(variable_data = variable_data, title = title, char_startDate = char_startDate, char_endDate = char_endDate, selected_station = selected_station, time_resolution = time_resolution))
}

## Function to prepare variable data and plot elements. Used for Spatial Interpolation module
GS_prepareVariableData <- function(selected_var, time_resolution, selected_time, weather_data) {
  variable_data <- NULL
  if (time_resolution == "Day") {
    selected_date <- as.Date(selected_time)
    variable_data <- weather_data %>% filter(Date == selected_date)
  } else if (time_resolution == "Month") {
    selected_month <- format(as.Date(selected_time), "%m")
    selected_year <- format(as.Date(selected_time), "%Y")
    variable_data <- weather_data %>% filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month))
  } else if (time_resolution == "Year") {
    selected_year <- as.character(format(selected_time, "%Y"))
    variable_data <- weather_data %>%filter(format(Date, "%Y") == selected_year)
  }
  # Group and summarise data based on the selected variable
  if (!is.null(variable_data)) {
    variable_data <- variable_data %>%
      group_by(Station, Latitude, Longitude) %>%
      summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop')
  } else {variable_data <- NULL}
  
  # Add legend_title to variable
  legend_title <- if(grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if(grepl("Temperature", selected_var)) {"Temperature (°C)"}
  
  # Update variable_data before converting to sf
  if (!is.null(variable_data)) {
    variable_data[[legend_title]] <- variable_data$ValueToPlot
    variable_data_sf <- st_as_sf(variable_data, coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(crs = 3414)
  } else {variable_data_sf <- NULL}
  time_title <- switch(time_resolution, "Day" = "Daily", "Month" = "Monthly", "Year" = "Yearly", NA)
  var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {if(time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
  date_title <- format(as.Date(selected_time), switch(time_resolution,"Day" = "%d %B %Y","Month" = "%B %Y","Year" = "%Y",NA))
  main_title <- paste(time_title, var_title, "for\n", date_title)
  
  palette <- if (grepl("Rainfall", selected_var)) {"Blues"} else if (grepl("Temperature", selected_var)) {"YlOrRd"}
  
  return(list(variable_data = variable_data, variable_data_sf = variable_data_sf, legend_title = legend_title, main_title = main_title, palette = palette, var_title = var_title))
}

# Function to get raster layer
GS_rasterlayer <- function(res){
  # Specify resolution
  bbox <- as.list(st_bbox(mpsz2019))
  nrows =  (bbox$ymax - bbox$ymin)/res
  ncols = (bbox$xmax - bbox$xmin)/res
  # Create raster layer, 'grid'
  grid <- rast(mpsz2019, nrows = nrows, ncols = ncols)
  # Generate coordinates for each cell of the raster
  xy <- xyFromCell(grid,1:ncell(grid))
  # Converting coordinates of raster into a spatial (sf) object
  coop <- st_as_sf(as.data.frame(xy), coords = c("x","y"), crs = st_crs(mpsz2019))
  # Filter to only only includes points within mpsz2019
  coop <- st_filter(coop, mpsz2019) 
  return(list(grid = grid, coop = coop))
}

# Section 2: Header and sidebar ----
header <- dashboardHeader(title = "Visual Analytics for Singapore's Weather",
                          titleWidth = 450)

sidebar <- dashboardSidebar(
  width = 170,
  tags$head(
    tags$style(HTML(
      ".sidebar-menu > li > a, .sidebar-menu .treeview-menu > li > a {
        white-space: normal; 
        line-height: 1.2;
        font-size: 12px;
      }
      /* Custom styles for the image */
      .sidebar-logo {
        padding: 10px;
        text-align: center; /* Center the image */
      }
      .sidebar-logo img {
        width: 80%; /* Adjust the image size */
        margin-bottom: 10px; /* Extra space below the image */
      }"
    ))
  ),
  # Use div with custom class for styling and placing the image
  div(class = "sidebar-logo",
      tags$img(src = "sunny.png", alt = "Logo")
  ),
  sidebarMenu(
    menuItem("Landing Page", tabName = "LandingPage", icon = icon("home")),
    menuItem("Exploratory & Confirmatory Data Analysis", tabName = "EDACDA", icon = icon("temperature-half")),
    menuItem("Univariate Forecasting", tabName = "Univariate", icon = icon("chart-line"),
             menuSubItem("Exploratory Time Series", tabName = "ExploreTS"),
             menuSubItem("Time Series Decomposition", tabName = "DecomposeTS"),
             menuSubItem("Forecasting", tabName = "ForecastTS")
    ),
    menuItem("Spatial Interpolation", tabName = "Geospatial", icon = icon("globe-asia"))
  )
)
# Section 3: CDA UI ----
CDAUI <- fluidPage(
  fluidRow(tabBox(
    title = "", width = 12, id = "CDA_tab", height = "250px",
    tabPanel("Compare across Stations",
             fluidRow(
               box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
                   selectInput("CDA_AS_selected_var", "Choose variable", variables, selected = "Mean Temperature (°C)", multiple = FALSE),
                   radioButtons("CDA_AS_time_resolution", label = "Select time resolution", c("Month", "Year")),
                   uiOutput("CDA_AS_dynamic_time_resolution"),
                   checkboxGroupInput("CDA_AS_selected_stations", "Select Station (s)", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1])
               ),
               tabBox(title = "", width = 10, id = "CDA_AS_tab", height = "250px",
                      tabPanel("Check Normality",
                               actionButton("CDA_AS_checknormality_button", "Run Normality Checks"),
                               plotOutput("CDA_AS_checknormality_plot"),
                               tableOutput("CDA_AS_checknormality_results")
                      ),
                      tabPanel("Run Statistic Test",
                               fluidRow(column(2, 
                                               selectInput("CDA_AS_selectedStatApproach", "Statistical Approach", choices = c("parametric", "nonparametric", "robust", "bayes"), selected = "nonparametric", multiple = FALSE),
                                               selectInput("CDA_AS_selectedConflevel", "Confidence Level", choices = c("90%"=0.90,"95%"=0.95, "99%"=0.99),selected ="0.95", multiple = FALSE),
                                               selectInput("CDA_AS_plotType", "Plot Type",choices = c("Boxviolin" = "boxviolin", "Box" = "box", "Violin" = "violin"),selected = "boxviolin",multiple = FALSE),
                                               textInput("CDA_AS_plot_title","Plot Title", placeholder = "Enter plot title"),
                                               actionButton("CDA_AS_plot_button", "Run test"),
                                               textInput("CDA_AS_Insights","Insights", placeholder = "Enter your insights"),
                                               actionButton("CDA_AS_Insights_button", "Save insights")
                               ),
                               column(10, plotlyOutput("CDA_AS_plot"), 
                                      uiOutput("CDA_AS_test_results"),
                                      verbatimTextOutput("CDA_AS_Insights_Output")
                               ))
                      )
               )
             )
    ),
    tabPanel("Compare across Time",
             fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
                          selectInput("CDA_AT_selected_var", "Choose variable", variables, selected = "Mean Temperature (°C)", multiple = FALSE),
                          selectInput("CDA_AT_selected_station", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1],  multiple = FALSE),
                          selectInput("CDA_AT_time_resolution", label = "Compare across", c("Years", "Months", "Months for a specified year", "Months of different years"), selected = NULL, multiple = FALSE),
                          uiOutput("CDA_AT_dynamic_time_resolution")
             ),
             tabBox(title = "", width = 10, id = "CDA_AT_tab", height = "250px",
                    tabPanel("Check Normality",
                             actionButton("CDA_AT_checknormality_button", "Run Normality Checks"),
                             plotOutput("CDA_AT_checknormality_plot"),
                             tableOutput("CDA_AT_checknormality_results")
                    ),
                    tabPanel("Run Statistic Test",
                             fluidRow(column(2, selectInput("CDA_AT_selectedStatApproach", "Statistical Approach", choices = c("parametric", "nonparametric", "robust", "bayes"),selected = "nonparametric", multiple = FALSE),
                                             selectInput("CDA_AT_selectedConflevel", "Confidence Level", choices = c("90%"=0.90,"95%"=0.95, "99%"=0.99),selected ="0.95", multiple = FALSE),
                                             selectInput("CDA_AT_plotType", "Plot Type",choices = c("Boxviolin" = "boxviolin", "Box" = "box", "Violin" = "violin"),selected = "boxviolin",multiple = FALSE),
                                             textInput("CDA_AT_plot_title","Plot Title", placeholder = "Enter plot title"),
                                             actionButton("CDA_AT_plot_button", "Run test"),
                                             textInput("CDA_AT_Insights","Insights", placeholder = "Enter your insights"),
                                             actionButton("CDA_AT_Insights_button", "Save insights")
                             ),
                             column(10, plotlyOutput("CDA_AT_plot"), 
                                    uiOutput("CDA_AT_test_results"),
                                    verbatimTextOutput("CDA_AT_Insights_Output")
                             )
                             )
                    )
                    
             )
             )
    )
  )
  )
)

# Section 4.1: ExploreTS UI ----
ExploreTSUI <- fluidPage(
  fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
               selectInput("ExploreTS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
               uiOutput("ExploreTS_dynamic_time_resolution"),
               checkboxGroupInput("ExploreTS_selectstation", "Select Station", choices = unique(weather_tsbl$Station),  selected = unique(weather_tsbl$Station)[1]),
               dateInput("ExploreTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-12-31", startview ="year"),
               dateInput("ExploreTS_endDate", "End Date", value = "2023-12-31", min = "2021-01-02", max = "2023-12-31", startview ="year"),
               uiOutput("ExporeTS_Date_Instructions")
  ),
  tabBox(title = "", width = 10,id = "ExploreTS_tab", height = "250px",
         tabPanel("Line graph",
                  plotlyOutput("ExploreTS_timeSeriesPlot"),
                  DT::dataTableOutput("ExploreTS_timeSeriesPlot_DataTable")
                  
         ),
         tabPanel("Horizon plot",
                  plotOutput("ExploreTS_HoriPlot")
         )
  )
  )
)

# Section 4.2: DecomposeTS UI ----
DecompTSUI <- fluidPage(
  fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
               selectInput("DecompTS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
               uiOutput("DecompTS_dynamic_time_resolution"),
               selectInput("DecompTS_selected_station", "Select Station", choices = unique(weather_tsbl$Station), selected = unique(weather_tsbl$Station)[1]),
               uiOutput("DecompTS_startDate_ui"),
               HTML("<div style='margin-top: 15px; margin-bottom: 15px;'>
  <strong>End Date (fixed)</strong><br>
  <div style='padding: 5px 10px; margin-top: 5px; display: inline-block; width: auto; color: #808080;'>
    2023-12-31
  </div>
</div>"),
               uiOutput("DecompTS_Date_Instructions"),
               HTML("<div style='margin-bottom: 10px;'></div>")
  ),
  
  
  tabBox(title = "", width = 10,id = "DecompTS_tab", height = "250px",
         tabPanel("ACF & PACF",
                  fluidRow(
                    column(3, title = "parameters",
                           uiOutput("DecompTS_dynamiclags")), 
                    column(9, plotlyOutput("DecompTS_ACFPlot", width = "100%", height = "300px"),
                           plotlyOutput("DecompTS_PACFPlot", width = "100%", height = "300px"))
                  )
         ),
         tabPanel("STL Decomposition",
                  fluidRow(
                    column(3, title = "parameters",
                           radioButtons("DecompTS_chooseautoSTL", label = "Use Auto STL?" ,c("Yes", "No")),
                           uiOutput("DecompTS_dynamic_autoSTL")),
                    column(9,plotlyOutput("DecompTS_STLPlot",width = "100%", height = "600px"))
                  )
         )
  )
  )
)

# Section 4.3: ForecastTS UI ----
ForecastTSUI <- fluidPage(
  # Row 1
  fluidRow(box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
               selectInput("ForecastTS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
               selectInput("ForecastTS_selected_station", "Select Station", choices = unique(weather_tsbl$Station), selected = unique(weather_tsbl$Station)[1]),
               uiOutput("ForecastTS_dynamic_time_resolution"),
               uiOutput("ForecastTS_startDate_ui"),
               HTML("<div style='margin-top: 15px; margin-bottom: 15px;'>
  <strong>End Date (fixed)</strong><br>
  <div style='padding: 5px 10px; margin-top: 5px; display: inline-block; width: auto; color: #808080;'>
    2023-12-31
  </div>
</div>"),
               uiOutput("ForecastTS_Date_Instructions"),
               HTML("<div style='margin-bottom: 10px;'></div>"),
               checkboxGroupInput("ForecastTS_selected_models", "Select Forecasting Models", choices = ForecastTS_model_choices),
               uiOutput("ForecastTS_dynamic_chooseautoSTL"),
               uiOutput("ForecastTS_dynamic_model_parameters"),
               sliderInput("ForecastTS_train_test_split", "Select Train-Test Split", min = 0.6, max = 1, value = 0.8, step = 0.1)
  ),
  tabBox(title = "", width = 10, id = "ForecastTS_tab", height = "250px",
         tabPanel("Model Calibration",
                  actionButton("ForecastTS_build_model", "Build Model"),
                  plotlyOutput("ForecastTS_forecast_validation_plot"),
                  fluidRow(column(6,plotlyOutput("ForecastTS_residual_plot", width= "100%", height = "280px")),
                           column(6, DT::dataTableOutput("ForecastTS_buildModel_DataTable"))
                  )
         ),
         tabPanel("Forecast Result",
                  fluidRow(
                    column(2,uiOutput("ForecastTS_dynamic_forecast_period"),
                           actionButton("ForecastTS_future_forecast", "Forecast")),
                    column(10, 
                           # uiOutput("ForecastTS_future_forecast_result")
                           plotlyOutput("ForecastTS_future_forecast_plot"),
                           DT::dataTableOutput("ForecastTS_future_forecast_DataTable")
                    )
                  )
         )
  )
  )
)


# Section 5: Geospatial UI ----

GeospatialUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("GS_selected_var", "Choose variable", variables_select, selected = "Mean Temperature (°C)", multiple = FALSE),
        selectInput("GS_time_resolution", "Time resolution", c("Day", "Month", "Year"), selected = "Month", multiple = FALSE),
        uiOutput("GS_dynamic_time_resolution")
    ),
    tabBox(
      title = "", width = 10, id = "Geospatial_tab", height = "auto",
      # The id lets us use input$Geospatial_tab on the server to find the current tab
      tabPanel("Map of stations",
               actionButton("GS_updatetmap", "Update map"), 
               uiOutput("GS_tmap_title"),
               tmapOutput("GS_tmap"),
               DT::dataTableOutput("GS_tmap_DataTable")
               
      ),
      tabPanel("Inverse Distance Weighted Interpolation Method",
               fluidRow(
                 column(3, 
                        sliderInput("GS_IDW_res", "res", min = 100, max = 300, value = 100, step = 50),
                        sliderInput("GS_IDW_nmax" , "nmax", min = 1, max = 10, value = 5),
                        actionButton("GS_updateIDW", "Show Result")
                 ),
                 column(9,plotOutput("GS_IDW_map", width = "100%", height = "600px"))
               )
      ),
      tabPanel("Ordinary Kriging Method",
               fluidRow(
                 column(2, 
                        sliderInput("GS_OK_res", "res", min = 100, max = 300, value = 100, step = 50),
                        selectInput("GS_OK_model" , "model", GS_model_choices, selected = "Gau"),
                        sliderInput("GS_OK_psill", "psill", min = 0.5, max = 10, value = 0.5, step = 0.5),
                        uiOutput("GS_OK_range_reactiveUI"),
                        sliderInput("GS_OK_nugget", "nugget", min = 0.1, max = 10, value = 0.1, step = 0.1),
                        actionButton("GS_updateOK", "Show Result")
                 ),
                 column(10,
                        fluidRow(
                          box(title =  tags$h4("Experimental and Fitted Variograms"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                              column(6,plotOutput("GS_OK_variogram", height = "400px")),
                              column(6,plotOutput("GS_OK_fitted_variogram", height = "400px"))
                          )
                        ),
                        fluidRow(uiOutput("GS_OK_result_title")),
                        fluidRow(
                          column(6,plotOutput("GS_OK_map",width = "100%", height = "500px")),
                          column(6,plotOutput("GS_OK_prediction_variance",width = "100%", height = "500px")))
                        
                 )
               )
      )
    )
  )
)

# Section 6: LandingPage UI ----
# Define UI
LandingPageUI <- fluidPage(
  fluidRow(
    column(width = 6,
           h2("A Visual Exploration Tool for Singapore's Weather"),
           p("Understanding Singapore's changing weather patterns is crucial, yet current tools for visualizing historical weather data are limited, often static, and lack depth. To fill this gap, we developed this interactive R Shiny application. Use this tool to explore and analyse Singapore's weather (2021-2023)!"),
           h3("Overview of modules in app"),
           tags$img(src = "overview.png", width = "100%"),
           p(strong("Exploratory & Confirmatory Data Analysis:"), 
             " Enables analysis of climate variables across stations and time periods, using statistical tests and interactive plots to identify weather patterns."),
           p(strong("Univariate Forecasting:"), 
             " Combines exploratory analysis, decomposition, and advanced forecasting with seasonal adjustment for accurate weather predictions across multiple stations."),
           p(strong("Spatial Interpolation:"), 
             " Generates isohyet/isotherm maps from sparse data, using interpolation techniques to estimate conditions at unmonitored locations, informed by data from Singapore's weather stations."),
           # p("The app consists of three key modules, each enabling the user to conduct a type of visual analysis with the dataset. The Exploratory & Confirmatory Data Analysis module allows users to conduct comparative analyses of weather data over different spatial and temporal scales. The Univariate Forecasting module provides a comprehensive toolkit of different forecasting models. The Spatial Interpolation module allows users to generate isohyet or isotherm maps to estimate rainfall/temperature at unmonitored locations. "),
           p("Click ", a("here", href = "https://isss608-group9-weatheranalytics.netlify.app/shinyapp/userguide.pdf", target = "_blank"), " to view the user guide.")
           
    ),
    column(3,
           uiOutput("hottestTemperatureInfo"),
           HTML("<div style='margin-bottom: 10px;'></div>"),
           uiOutput("wettestMonthInfo")),
    column(3,
           uiOutput("coolestTemperatureInfo"),
           HTML("<div style='margin-bottom: 10px;'></div>"),
           uiOutput("averageNumberOfRainyDaysInfo"))
  ),
  fluidRow(
    column(6, 
           h3("About the Dataset: Singapore Climate Records (2021-2023)"),
           p("The dataset comprises historical daily records of rainfall and temperature across 11 locations in Singapore, spanning from 2021 to 2023."),
           dataTableOutput("landingPageDataTable")),
    column(6, tmapOutput("stationsTmap"))
    
  )
)
# Section 7: Dashboard Body and UI ----
body <- dashboardBody(
  add_busy_spinner(spin = "fading-circle"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Merriweather+Sans:ital,wght@0,300..800;1,300..800&display=swap"),
    tags$style(HTML("
  body{
        font-family: 'Merriweather Sans', sans-serif;
        font-size: 14px; /* Set the default font size */
  }
  h1 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 2em; /* 36px if the base size is 16px */
  font-weight: 700; /* Bold */
  }
  h2 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 1.75em; /* 28px if the base size is 16px */
  font-weight: 700; /* Bold */
  }
  h3 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 1.25em; 
  font-weight: 600; /* Semi-Bold */
  }
  h4 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 1em; 
  font-weight: 600; /* Semi-Bold */
  }


      /* Customize the main header and navbar with a bold font */
      .skin-blue .main-header .logo, .skin-blue .main-header .logo:hover, .skin-blue .main-header, .skin-blue .main-header .navbar {
        background-color: #3D8DBC !important;
        font-family: 'Merriweather Sans', sans-serif;
        font-weight: 700; /* Bold */
      }
        .skin-blue .main-header .logo:hover {
          background-color: #3D8DBC;
          font-family: 'Merriweather Sans', sans-serif;
        }
        .skin-blue .main-header, .skin-blue .main-header .navbar {
      background-color: #3D8DBC!important;
        font-family: 'Merriweather Sans', sans-serif;
        }

      "))
  ),
  tabItems(
    tabItem(tabName = "LandingPage", LandingPageUI),
    tabItem(tabName = "EDACDA", h2("Exploratory & Confirmatory Data Analysis"), CDAUI),
    tabItem(tabName = "Univariate"),
    tabItem(tabName = "ExploreTS", h2("Exploring timeseries across stations"), ExploreTSUI),
    tabItem(tabName = "DecomposeTS",h2("Time Series Decomposition and ACF PACF plots"), DecompTSUI),
    tabItem(tabName = "ForecastTS",h2("Forecasting with different models"), ForecastTSUI),
    tabItem(tabName = "Geospatial",h2("Spatial Interpolation"), GeospatialUI)
  )
)

ui <- dashboardPage(header, sidebar, body)

# Section 8: Server code ----
server <- function(input, output, session) {
  
  # Landing Page ----
  
  output$landingPageDataTable <-DT::renderDataTable({
    
    tbl <- weather_data 
    tbl <- tbl %>% select(c(Station, Date, `Daily Rainfall Total (mm)`, `Mean Temperature (°C)`, `Maximum Temperature (°C)`, `Minimum Temperature (°C)`,  Latitude, Longitude))
    tbl$Station <-  as.factor(tbl$Station)
    datatable(tbl, 
              class= "hover",
              rownames = FALSE,
              width="80%", 
              filter = 'top',
              options = list(pageLength = 5, scrollX = TRUE))
  })
  
  
  output$stationsTmap <- renderTmap({
    
    unique_stations <- weather_data %>% 
      select(Station, Latitude, Longitude) %>% 
      distinct(Latitude, Longitude, .keep_all = TRUE)
    
    stations_sf <- st_as_sf(unique_stations, coords = c("Longitude", "Latitude"), crs = 4326)
    
    tm <-tm_basemap(server = "OpenStreetMap") + 
      tm_shape(stations_sf) +
      tm_dots(size = 0.2, col = '#9D6381', border.col = "#FDECEF",               # Set border color of dots to black
              border.lwd = 1) +  # Adjust size as needed
      tm_layout(title = "Station Locations in Singapore") +
      tm_view(set.view = c(lon = 103.8198, lat = 1.3521, zoom = 11)) # Centering on Singapore
    
    tm
    
  })
  
  output$hottestTemperatureInfo <- renderUI({
    # Find the row with the hottest temperature
    hottest_row <- weather_data[which.max(weather_data$`Maximum Temperature (°C)`), ]
    
    # Extract the hottest temperature, station, and date
    hottest_temperature <- hottest_row$`Maximum Temperature (°C)`
    station <- hottest_row$Station
    date <- hottest_row$Date
    # Define a nice light red color for the box fill
    box_fill_color <- "#ffcccc"
    
    tags$div(class = "panel panel-default", 
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      # Use a div with display flex to place the image next to the temperature
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "hot.png", height = "50px"), # Image from www folder
                               tags$h2(paste0(hottest_temperature, " °C"), style = "margin-left: 10px;") # Temperature next to the image
                      ),
                      tags$h3("Hottest Temperature Recorded:", style = "margin-top: 10px; text-align: center;") # "Hottest Temperature" text below
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Station:</strong> ", station, "</p>",
                                  "<p><strong>Date:</strong> ", date, "</p>")) # Station and date info in the panel body
             )
    )
  })
  
  output$coolestTemperatureInfo <- renderUI({
    # Find the row with the coolest temperature
    coolest_row <- weather_data[which.max(weather_data$`Minimum Temperature (°C)`), ]
    
    # Extract the coolest temperature, station, and date
    coolest_temperature <- coolest_row$`Minimum Temperature (°C)`
    station <- coolest_row$Station
    date <- coolest_row$Date
    # Define a nice light blue color for the box fill
    box_fill_color <- "#D7F9FF"
    
    # Create a box using Shiny's tags for custom HTML content
    tags$div(class = "panel panel-default",
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      # Use a div with display flex to place the image next to the temperature
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "cold.png", height = "50px"), # Image from www folder
                               tags$h2(paste0(coolest_temperature, " °C"), style = "margin-left: 10px;") # Temperature next to the image
                      ),
                      tags$h3("Coolest Temperature Recorded", style = "margin-top: 10px; text-align: center;") # "Coolest Temperature" text
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Station:</strong> ", station, "</p>",
                                  "<p><strong>Date:</strong> ", date, "</p>")) # Station and date info in the panel body
             )
    )
  })
  
  output$wettestMonthInfo <-renderUI({
    # Ensure weather_data$`Daily Rainfall Total (mm)` is numeric
    weather_data$`Daily Rainfall Total (mm)` <- as.numeric(as.character(weather_data$`Daily Rainfall Total (mm)`))
    
    # Aggregate rainfall by Year and Month to find total monthly rainfall
    monthly_rainfall <- weather_data %>%
      group_by(Year, Month) %>%
      summarise(Total_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE)) %>%
      ungroup()
    
    # Find the month with the highest total rainfall
    rainiest_month_info <- monthly_rainfall[which.max(monthly_rainfall$Total_Rainfall), ]
    
    
    # Assuming rainiest_month_info has been calculated as shown above
    
    # Extract information
    total_rainfall <- rainiest_month_info$Total_Rainfall
    year <- rainiest_month_info$Year
    month <- month.name[rainiest_month_info$Month] # Convert numeric month to name
    
    # Define a nice light blue color for the box fill
    box_fill_color <- "#A8D5E2"
    
    # Create a box using Shiny's tags for custom HTML content
    tags$div(class = "panel panel-default", 
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      # Flex display to place the image next to the heading
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "rain.png", height = "50px"), # Image from www folder
                               tags$h2(paste0(month, " ", year), style = "margin-left: 10px;") # Month and Year next to the image
                      ),
                      tags$h3("Rainiest Month Recorded", style = "margin-top: 10px; text-align: center;") # Heading text below image and date
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Total Rainfall (mm):</strong> ", total_rainfall, " mm</p>",
                                  "<p>(Across all stations in Singapore)")) # Rainfall info in the panel body
             )
    )
  })
  
  
  output$averageNumberOfRainyDaysInfo <-renderUI({
    # Ensure weather_data$`Daily Rainfall Total (mm)` is numeric
    weather_data$`Daily Rainfall Total (mm)` <- as.numeric(as.character(weather_data$`Daily Rainfall Total (mm)`))
    
    # Identify rainy days (>0 mm rainfall)
    weather_data <- mutate(weather_data, RainyDay = ifelse(`Daily Rainfall Total (mm)` > 0, 1, 0))
    
    # Aggregate to count rainy days per station and month
    rainy_days_per_month_station <- weather_data %>%
      group_by(Year, Month, Station) %>%
      summarise(RainyDays = sum(RainyDay, na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
    
    # Find the station and month with the highest number of rainy days
    max_rainy_days_info <- rainy_days_per_month_station[which.max(rainy_days_per_month_station$RainyDays), ]
    
    # Extract information
    total_rainy_days <- max_rainy_days_info$RainyDays
    year <- max_rainy_days_info$Year
    month <- month.name[max_rainy_days_info$Month] # Convert numeric month to name
    station_with_most_rainy_days <- max_rainy_days_info$Station
    
    # Define a nice light blue color for the box fill
    box_fill_color <- "#D3D4D9"
    
    # Create a box using Shiny's tags for custom HTML content
    tags$div(class = "panel panel-default", 
             style = paste0("background-color: ", box_fill_color, "; border-color: ", box_fill_color, ";"),
             tags$div(class = "panel-heading",
                      tags$div(style = "display: flex; align-items: center;",
                               tags$img(src = "kid.png", height = "50px"), # Image from www folder
                               tags$h2(total_rainy_days, style = "margin-left: 10px;")
                      ),
                      tags$h3("Highest number of Rainy Days", style = "margin-top: 10px; text-align: center;")
             ),
             tags$div(class = "panel-body",
                      HTML(paste0("<p><strong>Station:</strong> ", station_with_most_rainy_days, "</p>",
                                  "<p><strong>Month & Year:</strong> ", month, " ", year, "</p>"))
             )
    )
  })
  
  
  
  
  # CDA: Compare Across Stations ----
  
  # 1. Dynamic UI: CDA_AS_dynamic_time_resolution
  output$CDA_AS_dynamic_time_resolution <- renderUI ({
    if(input$CDA_AS_time_resolution == "Month") {
      airMonthpickerInput("CDA_AS_selected_date", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$CDA_AS_time_resolution == "Year") {
      airYearpickerInput("CDA_AS_selected_date", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  # 2. Normality Plot
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AS_checknormality_data_prep <- eventReactive(input$CDA_AS_checknormality_button, {
    req(length(input$CDA_AS_selected_stations) > 0)  # Ensure at least one station is selected    
    
    # Prepare data 
    result <- CDA_AS_prepareVariableData(input$CDA_AS_selected_var, input$CDA_AS_time_resolution, input$CDA_AS_selected_date, input$CDA_AS_selected_stations, weather_data)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    time_resolution <- result$time_resolution
    selected_stations <- input$selected_stations
    selected_date <- input$CDA_AS_selected_date # Take directly from input
    
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {selected_var}
    time_title <- if (input$CDA_AS_time_resolution == "Month") {paste(format(as.Date(selected_date), "%B"), format(as.Date(selected_date), "%Y"))} else if (input$CDA_AS_time_resolution == "Year") {as.character(format(selected_date, "%Y"))}
    title <- paste("Distribution of", var_title, "for", time_title)
    
    
    list(variable_data = variable_data, selected_var = selected_var, time_resolution = time_resolution, selected_stations = selected_stations, selected_date = selected_date, title = title)
  })
  
  
  ## Output normality plot
  output$CDA_AS_checknormality_plot <- renderPlot({
    
    result <- CDA_AS_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    time_resolution <- result$time_resolution
    selected_date <- result$selected_date
    title <- result$title
    
    ggplot(data = variable_data,
           aes(x = !!var_symbol, y = Station)) +
      geom_density_ridges(fill =     if (grepl("Rainfall", selected_var)) {"lightblue"} else if (grepl("Temperature", selected_var)) {"#FEC26B"}, alpha = 0.9) +
      labs(title = title) +
      theme_ridges()+
      theme(legend.position = "none", axis.title.y = element_blank())   
  })
  
  ## Output table of normality test results 
  output$CDA_AS_checknormality_results <- renderTable({
    
    result <- CDA_AS_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    
    ad_results <- variable_data %>%
      group_by(Station) %>%
      summarise_at(vars(!!var_symbol), list(
        `Anderson-Darling Statistic` = ~ad.test(.)$statistic,
        `Anderson-Darling p.value` = ~ad.test(.)$p.value,
        `Shapiro-Wilk Statistic` = ~shapiro.test(.)$statistic,
        `Shapiro-Wilk p.value` = ~shapiro.test(.)$p.value
      )) %>%
      mutate(across(where(is.numeric), ~format(round(., 4), nsmall = 4))) %>%
      ungroup()
    ad_results
  })
  
  #3. Comparison Plot
  
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AS_caption_reactive <- reactiveValues() # Initializes a reactive value FOR CAPTION
  
  CDA_AS_data_prep <- eventReactive(input$CDA_AS_plot_button, {
    req(length(input$CDA_AS_selected_stations) > 0)  # Ensure at least one station is selected    
    
    # Prepare data 
    result <- CDA_AS_prepareVariableData(input$CDA_AS_selected_var, input$CDA_AS_time_resolution, input$CDA_AS_selected_date, input$CDA_AS_selected_stations, weather_data)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    var_symbol <- rlang::sym(selected_var)
    time_resolution <- result$time_resolution
    # selected_stations <- input$selected_stations
    selected_date <- input$CDA_AS_selected_date # Take directly from input
    title <- input$CDA_AS_plot_title # Take directly from input
    selectedStatApproach <- input$CDA_AS_selectedStatApproach
    selectedConflevel <- input$CDA_AS_selectedConflevel
    plotType <- input$CDA_AS_plotType
    
    p<-ggbetweenstats(data = variable_data, x = "Station", y = !!var_symbol, 
                      type = selectedStatApproach,
                      mean.ci = TRUE,
                      conf.level = as.integer(selectedConflevel),
                      violin.args = if(plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} 
                      else {list(trim=TRUE,alpha = 0.2)},
                      boxplot.args = if(plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} 
                      else {list(alpha = 0.2)},
                      pairwise.comparisons = TRUE, 
                      pairwise.annotation = TRUE,
                      pairwise.display = "s", 
                      sig.level = NA,
                      p.adjust.method = "fdr",
                      messages = FALSE,
                      title = title)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Extract stats and store in reactive variable
    test_stats <- extract_stats(p)
    
    # Extract Caption
    if (n_distinct(variable_data$Station) > 1){
      caption <- paste(test_stats$subtitle_data[["method"]][1], ":", test_stats$subtitle_data[["statistic"]][1], ", p-value:", test_stats$subtitle_data[["p.value"]][1])
      CDA_AS_caption_reactive$caption <- caption
    } else{CDA_AS_caption_reactive$caption <- NULL}
    
    # Compute distribution statistics by y_axis
    if (selectedStatApproach == "parametric"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "mean")[1])
    } else if (selectedStatApproach == "nonparametric"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "median")[1])
    }else if (selectedStatApproach == "robust"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "trimmed")[1])
    }else if (selectedStatApproach == "bayes"){
      distribution_stats <- variable_data %>%
        group_by(Station) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "MAP")[1])
    }
    
    CDA_AS_caption_reactive$centrality_measure <- as.data.frame(distribution_stats)
    
    # Extract pairwise comparison
    if (n_distinct(variable_data$Station) > 2){
      # print(test_stats$pairwise_comparisons_data)
      CDA_AS_caption_reactive$pairwise_table <- as.data.frame(test_stats$pairwise_comparisons_data) %>% select(-expression)
    } else { CDA_AS_caption_reactive$pairwise_table <-NULL}
    
    list(variable_data = variable_data, selected_var = selected_var,time_resolution = time_resolution, title = title,
         selectedStatApproach=selectedStatApproach,selectedConflevel=selectedConflevel, plotType = plotType,
         p = p, CDA_AS_caption_reactive = CDA_AS_caption_reactive
    )
  })
  
  ## Output plot
  output$CDA_AS_plot <- renderPlotly({
    # Extract the result from the eventReactive object
    result <- CDA_AS_data_prep()
    p <-result$p
    return(p)
  })
  
  
  ## Output test statistics
  
  output$CDA_AS_Caption_Output <- renderText({
    # Extract the result from the eventReactive object
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if (!is.null(CDA_AS_caption_reactive$caption)) {
      CDA_AS_caption_reactive$caption
    }
  })
  
  output$CDA_AS_centrality_measure_title <- renderUI({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if (!is.null(CDA_AS_caption_reactive$centrality_measure)) {
      tags$h4("\nCentrality Measures")
    }
  })
  
  output$CDA_AS_centrality_measure_datatable <- renderTable({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if (!is.null(CDA_AS_caption_reactive$centrality_measure)) {
      CDA_AS_caption_reactive$centrality_measure
    }
  })
  
  output$CDA_AS_pairwise_comparison_title <- renderUI({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <-result$CDA_AS_caption_reactive
    if(!is.null(CDA_AS_caption_reactive$pairwise_table)){
      tags$h4("\nPairwise Comparison")
    }
  })
  
  output$CDA_AS_pairwise_comparison_datatable <- renderTable({
    result <- CDA_AS_data_prep()
    CDA_AS_caption_reactive <- result$CDA_AS_caption_reactive
    if(!is.null(CDA_AS_caption_reactive$pairwise_table)){
      CDA_AS_caption_reactive$pairwise_table
    }
  })
  
  output$CDA_AS_test_results <- renderUI({
    if (!is.null(CDA_AS_caption_reactive)) {
      tagList(
        textOutput("CDA_AS_Caption_Output"),
        uiOutput("CDA_AS_centrality_measure_title"),
        tableOutput("CDA_AS_centrality_measure_datatable"),
        uiOutput("CDA_AS_pairwise_comparison_title"),
        tableOutput("CDA_AS_pairwise_comparison_datatable")
      )
    }
  })
  
  ## Output insights
  AS_insightsText <- eventReactive(input$CDA_AS_Insights_button,{
    input$CDA_AS_Insights
  }, ignoreNULL = TRUE)
  output$CDA_AS_Insights_Output <- renderText({ AS_insightsText() })
  
  
  # CDA: Compare Across Time ----
  # 1. Dynamic UI
  output$CDA_AT_dynamic_time_resolution <- renderUI ({
    
    if (input$CDA_AT_time_resolution == "Years") {
      checkboxGroupInput("CDA_AT_selected_dates", label = "Select years", choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1])
    } else if (input$CDA_AT_time_resolution == "Months") {
      checkboxGroupInput("CDA_AT_selected_dates", label = "Select month", choices = unique(weather_data$Month_Name))      
    } else if (input$CDA_AT_time_resolution == "Months for a specified year") {
      list(selectInput("CDA_AT_selected_year", label = "Select year", choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1], multiple = FALSE),
           checkboxGroupInput("CDA_AT_selected_dates", label = "Select month", choices = unique(weather_data$Month_Name)) 
      )
    } else if (input$CDA_AT_time_resolution == "Months of different years") {
      list(checkboxGroupInput("CDA_AT_selected_dates", label = "Select year", choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1]),
           selectInput("CDA_AT_selected_month", label = "Select month", choices = unique(weather_data$Month_Name), multiple = FALSE) 
      )
    }
  })
  
  # 2. Normality Plot
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AT_checknormality_data_prep <- eventReactive(input$CDA_AT_checknormality_button, {
    req(length(input$CDA_AT_selected_dates) > 0)  # Ensure at least one "date" is selected
    
    selected_var <- input$CDA_AT_selected_var
    time_resolution <- input$CDA_AT_time_resolution
    selected_station <- input$CDA_AT_selected_station
    
    if (time_resolution == "Years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months for a specified year") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_year <- input$CDA_AT_selected_year
      
      variable_data <- weather_data %>%
        filter(Year %in% selected_year,
               Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months of different years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_month <- input$CDA_AT_selected_month
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Month_Name %in% selected_month,
               Station %in% selected_station)
    }
    
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {selected_var}
    time_title <- if(time_resolution == "Months") {"across months"} else if (time_resolution == "Years") {"across years"} 
    else if(time_resolution =="Months for a specified year") {paste("across months of", input$CDA_AT_selected_year)}
    else if (time_resolution == "Months of different years"){paste("for", input$CDA_AT_selected_month, "across years")}
    
    title <- paste("Distribution of", var_title, time_title, "for", selected_station)
    
    y_axis <- if(time_resolution == "Months") {"Month_Name"} else if (time_resolution == "Years") {"Year"} 
    else if(time_resolution =="Months for a specified year") {"Month_Name"}
    else if (time_resolution == "Months of different years"){"Year"}
    
    list(variable_data = variable_data, selected_var = selected_var, y_axis = y_axis, time_resolution = time_resolution, selected_station = selected_station, title = title)
  })
  
  ## Output normality plot
  output$CDA_AT_checknormality_plot <- renderPlot({
    
    result <- CDA_AT_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    selected_var <- result$selected_var
    time_resolution <- result$time_resolution
    title <- result$title
    y_axis <- result$y_axis
    
    
    ggplot(data = variable_data, aes(x = !!var_symbol, y = as.factor(!!rlang::sym(y_axis)))) +
      geom_density_ridges(fill =if (grepl("Rainfall", selected_var)) {"lightblue"} else if (grepl("Temperature", selected_var)) {"#FEC26B"}, alpha = 0.9) +
      labs(title = title) + 
      theme_ridges() +
      theme(legend.position = "none", axis.title.y = element_blank())  
  })
  
  ## Output table of normality test results 
  output$CDA_AT_checknormality_results <- renderTable({
    
    result <- CDA_AT_checknormality_data_prep()
    
    var_symbol <- rlang::sym(result$selected_var)
    variable_data <- result$variable_data
    y_axis <- result$y_axis
    # time_resolution <- result$time_resolution
    
    ad_results <- variable_data %>%
      group_by(Group = as.factor(!!rlang::sym(y_axis))) %>%
      summarise(
        `Anderson-Darling Statistic` = list(ad.test(get(var_symbol))$statistic),
        `Anderson-Darling p.value` = list(ad.test(get(var_symbol))$p.value),
        `Shapiro-Wilk Statistic` = list(shapiro.test(get(var_symbol))$statistic),
        `Shapiro-Wilk p.value` = list(shapiro.test(get(var_symbol))$p.value)
      ) %>%
      unnest(cols = c(`Anderson-Darling Statistic`, `Anderson-Darling p.value`, `Shapiro-Wilk Statistic`, `Shapiro-Wilk p.value`)) %>%
      mutate(across(where(is.numeric), ~format(round(., 4), nsmall = 4))) %>%
      ungroup() %>%
      setNames(c(y_axis, names(.)[-1]))  # Dynamically rename the first column
    
    if (y_axis == "Month_Name") {
      ad_results <- ad_results %>% rename(Month = Month_Name)
    }
    
    ad_results
  })
  
  #3. Comparison Plot
  ## Prepare data and create eventReactive object that updates only when button is clicked
  CDA_AT_caption_reactive <- reactiveValues() # Initializes a reactive value FOR CAPTION
  CDA_AT_data_prep <- eventReactive(input$CDA_AT_plot_button, {
    req(length(input$CDA_AT_selected_dates) > 0)  # Ensure at least one "date" is selected
    
    selected_var <- input$CDA_AT_selected_var
    time_resolution <- input$CDA_AT_time_resolution
    selected_station <- input$CDA_AT_selected_station
    title <- input$CDA_AT_plot_title # Take directly from input
    var_symbol <- rlang::sym(selected_var)
    selected_dates <- input$CDA_AT_selected_dates
    
    if (time_resolution == "Years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months") {
      
      selected_dates <- input$CDA_AT_selected_dates
      variable_data <- weather_data %>%
        filter(Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months for a specified year") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_year <- input$CDA_AT_selected_year
      
      variable_data <- weather_data %>%
        filter(Year %in% selected_year,
               Month_Name %in% selected_dates,
               Station %in% selected_station)
      
    } else if (time_resolution == "Months of different years") {
      
      selected_dates <- input$CDA_AT_selected_dates
      selected_month <- input$CDA_AT_selected_month
      variable_data <- weather_data %>%
        filter(Year %in% selected_dates,
               Month_Name %in% selected_month,
               Station %in% selected_station)
    }
    
    y_axis <- if(time_resolution == "Months") {"Month_Name"} else if (time_resolution == "Years") {"Year"} 
    else if(time_resolution =="Months for a specified year") {"Month_Name"}
    else if (time_resolution == "Months of different years"){"Year"}
    
    selectedStatApproach<-input$CDA_AT_selectedStatApproach
    selectedConflevel<-input$CDA_AT_selectedConflevel
    plotType<-input$CDA_AT_plotType
    
    
    p <- ggbetweenstats(data = variable_data,
                        x = !!rlang::sym(y_axis),
                        y = !!var_symbol,
                        type = selectedStatApproach,
                        mean.ci = TRUE,
                        centrality.plotting = TRUE,
                        conf.level = as.integer(selectedConflevel),
                        violin.args = if(plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} else {list(trim=TRUE,alpha = 0.2)},
                        boxplot.args = if(plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} else {list(alpha = 0.2)},
                        pairwise.comparisons = TRUE,
                        pairwise.annotation = FALSE,
                        pairwise.display = "s",
                        sig.level = NA,
                        p.adjust.method = "fdr",
                        title = title,
                        messages = FALSE)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Extract stats and store in reactive variable
    test_stats <- extract_stats(p)
    
    # Extract Caption
    if (length(selected_dates) > 1){
      caption <- paste(test_stats$subtitle_data[["method"]][1], ":", test_stats$subtitle_data[["statistic"]][1], ", p-value:", test_stats$subtitle_data[["p.value"]][1])
      CDA_AT_caption_reactive$caption <- caption
    } else {      CDA_AT_caption_reactive$caption <- NULL}
    
    # Compute distribution statistics by y_axis
    if (selectedStatApproach == "parametric"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "mean")[1])
      CDA_AT_caption_reactive$centrality_measure <- as.data.frame(distribution_stats)
    } else if (selectedStatApproach == "nonparametric"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "median")[1])
    }else if (selectedStatApproach == "robust"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "trimmed")[1])
    }else if (selectedStatApproach == "bayes"){
      distribution_stats <- variable_data %>%
        group_by(!!rlang::sym(y_axis)) %>%
        summarise(describe_distribution(!!var_symbol, centrality = "MAP")[1])
    }
    CDA_AT_caption_reactive$centrality_measure <- as.data.frame(distribution_stats)
    
    
    # Extract pairwise comparison
    if (length(selected_dates) > 2){
      CDA_AT_caption_reactive$pairwise_table <- as.data.frame(test_stats$pairwise_comparisons_data) %>% select(-expression)
    } else { CDA_AT_caption_reactive$pairwise_table <-NULL}
    
    list(variable_data = variable_data, selected_var = selected_var, y_axis = y_axis, time_resolution = time_resolution, selected_station = selected_station, selected_dates = selected_dates, title = title,
         selectedStatApproach=selectedStatApproach,selectedConflevel=selectedConflevel, plotType = plotType, 
         p = p, CDA_AT_caption_reactive = CDA_AT_caption_reactive)
  })
  
  ## Output plot
  
  output$CDA_AT_plot <- renderPlotly({
    
    # Extract the result from the eventReactive object
    result <- CDA_AT_data_prep()
    p <-result$p
    
    return(p)
  })
  
  ## Output test statistics
  
  output$CDA_AT_Caption_Output <- renderText({
    # Extract the result from the eventReactive object
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if (!is.null(CDA_AT_caption_reactive$caption)) {
      CDA_AT_caption_reactive$caption
    }
  })
  
  output$CDA_AT_centrality_measure_title <- renderUI({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if (!is.null(CDA_AT_caption_reactive$centrality_measure)) {
      tags$h4("\nCentrality Measures")
    }
  })
  
  output$CDA_AT_centrality_measure_datatable <- renderTable({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if (!is.null(CDA_AT_caption_reactive$centrality_measure)) {
      CDA_AT_caption_reactive$centrality_measure
    }
  })
  
  output$CDA_AT_pairwise_comparison_title <- renderUI({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <-result$CDA_AT_caption_reactive
    if(!is.null(CDA_AT_caption_reactive$pairwise_table)){
      tags$h4("\nPairwise Comparison")
    }
  })
  
  output$CDA_AT_pairwise_comparison_datatable <- renderTable({
    result <- CDA_AT_data_prep()
    CDA_AT_caption_reactive <- result$CDA_AT_caption_reactive
    if(!is.null(CDA_AT_caption_reactive$pairwise_table)){
      CDA_AT_caption_reactive$pairwise_table
    }
  })
  
  output$CDA_AT_test_results <- renderUI({
    if (!is.null(CDA_AT_caption_reactive)) {
      tagList(
        textOutput("CDA_AT_Caption_Output"),
        uiOutput("CDA_AT_centrality_measure_title"),
        tableOutput("CDA_AT_centrality_measure_datatable"),
        uiOutput("CDA_AT_pairwise_comparison_title"),
        tableOutput("CDA_AT_pairwise_comparison_datatable")
      )
    }
  })
  
  
  ## Output insights
  AT_insightsText <- eventReactive(input$CDA_AT_Insights_button,{
    input$CDA_AT_Insights
  }, ignoreNULL = TRUE)
  output$CDA_AT_Insights_Output <- renderText({ AT_insightsText() })
  
  # Time Series: Exploratory Plots ----
  
  ## 1. Dynamic UI for ExploreTS
  
  ### Dynamic UI  For time resolution
  output$ExploreTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Week", "Month"))
    } else if (grepl("Temperature", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Day" ,"Week", "Month"))
    }
  })
  
  
  ### Update input start date and end date
  observe({
    start_date <- input$ExploreTS_startDate
    end_date <- input$ExploreTS_endDate
    # Calculate the minimum end date by adding one month to the start date
    min_end_date <- start_date %m+% months(1) # Adjust to get the day before a full month, ensuring a full month period
    
    # If the end date is less than the minimum end date, adjust it
    if (end_date < start_date || end_date < min_end_date) {
      # If the end date is selected before the start date, adjust the start date instead
      if (end_date < start_date) {
        # Ensure that adjusting the start date backward does not go before a minimum allowed date
        min_allowed_start_date <- as.Date("2021-01-01")
        new_start_date <- end_date %m-% months(1) + days(1) # Adjust to ensure a full month period from end date
        if (new_start_date < min_allowed_start_date) {
          new_start_date <- min_allowed_start_date
        }
        # Update the start date input to ensure a minimum 1 month period
        updateDateInput(session, "ExploreTS_startDate", value = new_start_date)
      } else {
        # Adjust the end date to ensure a minimum duration of 1 month
        if (min_end_date > as.Date("2023-12-31")) {
          # If the new minimum end date exceeds the maximum allowed date, adjust it to the maximum
          min_end_date <- as.Date("2023-12-31")
        }
        # Update the end date input to ensure a minimum 1 month period
        updateDateInput(session, "ExploreTS_endDate", value = min_end_date)
      }
    }
    
  })
  
  ### Instructions
  output$ExporeTS_Date_Instructions <- renderUI({
    HTML("<em>Please select a period of at least 1 month for plotting</em>")
  })
  ## 2. Prepare data and plot line graph
  ### Output Plot
  ExploreTS_timeSeriesPlot_DataTable_reactive <- reactiveVal()
  output$ExploreTS_timeSeriesPlot <- renderPlotly({
    
    req(length(input$ExploreTS_selectstation) > 0)  # Ensure stations are selected
    # Prepare Data
    selected_var <- input$ExploreTS_selected_var
    
    char_startDate <- as.character(input$ExploreTS_startDate)
    char_endDate <- as.character(input$ExploreTS_endDate)
    
    variable_data <- weather_tsbl %>%
      filter_index(char_startDate ~ char_endDate) %>%
      filter(Station %in% input$ExploreTS_selectstation)
    
    if (input$ExploreTS_time_resolution == "Day") {
      variable_data <- variable_data %>%
        mutate(ValueToPlot = .data[[selected_var]])
    } else if (input$ExploreTS_time_resolution == "Week") {
      variable_data <- variable_data %>%
        group_by_key() %>%
        index_by(year_week = ~ yearweek(.)) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    } else if (input$ExploreTS_time_resolution == "Month") {
      variable_data <- variable_data %>%
        as_tibble() %>%
        group_by(Station, Year, Month) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
    }
    
    # Store in reactive val for datatable
    ExploreTS_timeSeriesPlot_DataTable_reactive(variable_data)
    
    # Update plot annotations
    yaxis_title <- if(grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if(grepl("Temperature", selected_var )) {"Temperature (°C)"}
    time_title <- switch(input$ExploreTS_time_resolution,"Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {
      if(input$ExploreTS_time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
    title <- paste(time_title, var_title, "\n", char_startDate, "to", char_endDate)
    
    #Create plotly plot
    plot_ly(variable_data, x = ~Date, y = ~ValueToPlot, 
            type = 'scatter', mode = 'lines', 
            color = ~Station, hoverinfo = 'text',
            text = ~paste("<b>Station:</b>", Station,
                          "<br>", 
                          ifelse(input$ExploreTS_time_resolution == "Week", "<b>Week starting:</b> ", 
                                 ifelse(input$ExploreTS_time_resolution == "Month", "<b>Month:</b> ", "<b>Date:</b> ")), 
                          ifelse(input$ExploreTS_time_resolution == "Month", format(Date, "%Y %b"), as.character(Date)),
                          "<br><b>", ifelse(grepl("Rainfall", selected_var), "Total Rainfall (mm)", selected_var), ":</b> ", ValueToPlot)) %>%
      layout(title = title,
             xaxis = list(title = "", 
                          range = c(char_startDate, char_endDate), 
                          rangeslider = list(type = "date", range = c(char_startDate, char_endDate))),
             yaxis = list(title = yaxis_title))
  })
  
  ### Output datatable
  output$ExploreTS_timeSeriesPlot_DataTable <- DT::renderDataTable({
    req(length(input$ExploreTS_selectstation) > 0)
    
    
    data_frame <- as.data.frame(ExploreTS_timeSeriesPlot_DataTable_reactive())
    print(data_frame)
    
    data_frame$Station <-  as.factor(data_frame$Station)
    
    if (input$ExploreTS_time_resolution == "Week") {
      data_frame <- data_frame %>% select(Station, Date, year_week, ValueToPlot)
      data_frame$year_week <- as.character(data_frame$year_week)
      data_frame <- data_frame %>%
        rename(Week := year_week)
    } else if (input$ExploreTS_time_resolution == "Month"){
      data_frame <- data_frame %>% select(Station, Date, Month, ValueToPlot)
      data_frame <- data_frame %>%
        mutate(Month = month.name[Month])
    } else if (input$ExploreTS_time_resolution == "Day") {
      data_frame <- data_frame %>% select(Station, Date, ValueToPlot)
      
    }
    
    data_frame <- data_frame %>%
      rename(!!input$ExploreTS_selected_var := ValueToPlot)
    # data_frame <- data_frame %>%
    #   rename_with(~"ValueToPlot", .cols = all_of(input$ExploreTS_selected_var))
    
    datatable(data_frame, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              filter = 'top',
              options = list(pageLength = 10, scrollX = TRUE))
    
  })
  
  ## 3. Plot horizon plot 
  output$ExploreTS_HoriPlot <- renderPlot({
    req(length(input$ExploreTS_selectstation) > 0)
    # Prepare Data
    selected_var <- input$ExploreTS_selected_var
    
    char_startDate <- as.character(input$ExploreTS_startDate)
    char_endDate <- as.character(input$ExploreTS_endDate)
    
    variable_data <- weather_tsbl %>%
      filter_index(char_startDate ~ char_endDate) %>%
      filter(Station %in% input$ExploreTS_selectstation)
    
    if (input$ExploreTS_time_resolution == "Day") {
      variable_data <- variable_data %>%
        mutate(ValueToPlot = .data[[selected_var]])
    } else if (input$ExploreTS_time_resolution == "Week") {
      variable_data <- variable_data %>%
        group_by_key() %>%
        index_by(year_week = ~ yearweek(.)) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    } else if (input$ExploreTS_time_resolution == "Month") {
      variable_data <- variable_data %>%
        as_tibble() %>%
        group_by(Station, Year, Month) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
    }
    
    # Store in reactive val for datatable
    ExploreTS_timeSeriesPlot_DataTable_reactive(variable_data)
    
    # Update plot annotations
    yaxis_title <- if(grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if(grepl("Temperature", selected_var )) {"Temperature (°C)"}
    time_title <- switch(input$ExploreTS_time_resolution,"Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {
      if(input$ExploreTS_time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
    title <- paste(time_title, var_title, "\n", char_startDate, "to", char_endDate)
    
    # Compute origin and  horizon scale cutpoints: 
    cutpoints <- variable_data %>% 
      mutate(
        outlier = between(
          .data[["ValueToPlot"]], 
          quantile(.data[["ValueToPlot"]], 0.25, na.rm = TRUE) -
            1.5 * IQR(.data[["ValueToPlot"]], na.rm = TRUE),
          quantile(.data[["ValueToPlot"]], 0.75, na.rm = TRUE) +
            1.5 * IQR(.data[["ValueToPlot"]], na.rm = TRUE))) %>% 
      filter(outlier)
    
    
    ori <- sum(range(cutpoints[["ValueToPlot"]]))/2
    sca <- seq(range(cutpoints[["ValueToPlot"]])[1], range(cutpoints[["ValueToPlot"]])[2], length.out = 7)[-4]
    
    
    ori <- round(ori, 2) # The origin, rounded to 2 decimal places
    sca <- round(sca, 2) # The horizon scale cutpoints
    
    # reverse_T <- ifelse(grepl("Rainfall", selected_var), FALSE, TRUE)
    palette <- if (grepl("Rainfall", selected_var)) {"Blues"} else if (grepl("Temperature", selected_var)) {"YlOrRd"}
    # Plot horizon plot
    variable_data %>% ggplot() +
      geom_horizon(aes(x = Date, 
                       y = .data[["ValueToPlot"]],
                       fill = after_stat(Cutpoints)), 
                   origin = ori, horizonscale = sca) +
      scale_fill_hcl(palette = palette, reverse = T) +
      facet_grid(Station ~ .)+
      theme_few() +
      theme(
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()
      ) +
      scale_x_date(expand = c(0, 0), 
                   date_breaks = "1 month", 
                   date_labels = "%b") +
      xlab('Date') +
      ggtitle(title)
    
    
  })
  
  
  # Time Series: Decomposition ----
  
  ## 1.1 Dynamic UI for DecompTS_dynamic_time_resolution
  output$DecompTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$DecompTS_selected_var)) {
      radioButtons("DecompTS_time_resolution", label = "Select time resolution", c("Week"))
    } else if (grepl("Temperature", input$DecompTS_selected_var)) {
      radioButtons("DecompTS_time_resolution", label = "Select time resolution", c("Day" ,"Week"))
    }
  })
  
  output$DecompTS_startDate_ui <- renderUI({
    if(input$DecompTS_time_resolution == "Day"){
      dateInput("DecompTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-11-30")
    } else if (input$DecompTS_time_resolution == "Week"){
      dateInput("DecompTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-06-25")
    }
    
    
  })
  
  ### Instructions
  output$DecompTS_Date_Instructions <- renderUI({
    
    if(input$DecompTS_time_resolution == "Day"){
      HTML("<em>Please select a period of at least 30 days for plotting. </em>")
    } else if (input$DecompTS_time_resolution == "Week"){
      HTML("<em>Please select a period of at least 26 weeks for plotting. </em>")    }
    
  })
  
  ## 1.2 Dynamic UI for DecompTS_dynamiclags
  output$DecompTS_dynamiclags <- renderUI ({
    if (input$DecompTS_time_resolution == "Day") {
      sliderInput("DecompTS_lags", "Number of Lags", min = 1, max = 365, value = 20, step = 1)
    } else if (input$DecompTS_time_resolution == "Week") {
      sliderInput("DecompTS_lags", "Number of Lags", min = 1, max = 52, value = 20, step = 1)
    }
  })
  ## 1.3 Dynamic UI for DecompTS_dynamic_autoSTL
  output$DecompTS_dynamic_autoSTL <- renderUI({
    if (input$DecompTS_chooseautoSTL == "No") {
      if (input$DecompTS_time_resolution == "Day") {
        list(sliderInput("DecompTS_TrendWindow", "Trend Window", min = 1, max = 365, value = 20, step = 1),
             sliderInput("DecompTS_SeasonWindow", "Season Window", min = 1, max = 365, value = 20, step = 1))
      } else if (input$DecompTS_time_resolution == "Week") {
        list(sliderInput("DecompTS_TrendWindow", "Trend Window", min = 1, max = 52, value = 20, step = 1),
             sliderInput("DecompTS_SeasonWindow", "Season Window", min = 1, max = 52, value = 20, step = 1))
      }
    }
  })
  
  
  ## 2. ACF Plot and PACF Plot
  ### ACF plot
  output$DecompTS_ACFPlot <- renderPlotly({
    
    result <- TS_prepareVariableData(input$DecompTS_selected_var,as.character(input$DecompTS_startDate),"2023-12-31",input$DecompTS_selected_station,input$DecompTS_time_resolution,weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    char_startDate <- result$char_startDate
    char_endDate <- result$char_endDate
    
    ACF <- variable_data %>%
      ACF(ValueToPlot, lag_max = input$DecompTS_lags) %>%
      autoplot() +
      labs(title = paste("ACF plot of", title,  "\n", char_startDate, "to", char_endDate)) +
      theme_minimal()
    
    ggplotly(ACF)
  })
  ### PACF plot
  output$DecompTS_PACFPlot <- renderPlotly({ 
    result <- TS_prepareVariableData(input$DecompTS_selected_var,as.character(input$DecompTS_startDate),"2023-12-31",input$DecompTS_selected_station, input$DecompTS_time_resolution,weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    char_startDate <- result$char_startDate
    char_endDate <- result$char_endDate
    
    PACF <- variable_data %>%
      PACF(ValueToPlot, lag_max = input$DecompTS_lags) %>%
      autoplot() +
      labs(title = paste("PACF plot of", title,  "\n", char_startDate, "to", char_endDate)) +
      theme_minimal()
    
    ggplotly(PACF)
  })
  
  # 3. DecompTS_STLPlot
  output$DecompTS_STLPlot <- renderPlotly({
    result <- TS_prepareVariableData(input$DecompTS_selected_var,as.character(input$DecompTS_startDate),"2023-12-31", input$DecompTS_selected_station,input$DecompTS_time_resolution,weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    char_startDate <- result$char_startDate
    char_endDate <- result$char_endDate
    
    if(input$DecompTS_chooseautoSTL == "No") {
      trend_window <- input$DecompTS_TrendWindow
      season_window <- input$DecompTS_SeasonWindow
    } else { 
      trend_window <- NULL
      season_window <- NULL
    }
    
    # Note that variable name is always 'ValueToPlot', because of the reactive variable. So, use it directly in STL
    stl_fit <- variable_data %>%
      model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window))) %>%
      components()
    
    # Convert to tibble and perform common transformations
    stl_fit_tibble <- as_tibble(stl_fit) %>%
      mutate(trend = round(trend, 2),
             season_adjust = round(season_adjust, 2),
             season_year = round(season_year, 4),
             remainder = round(remainder, 4))
    
    # Conditionally mutate season_week if the resolution is "Day"
    if(input$DecompTS_time_resolution == "Day") {
      stl_fit_tibble <- stl_fit_tibble %>%
        mutate(season_week = round(season_week, 4))
    }
    
    # Visualize the STL components
    plot_stl <- stl_fit %>%
      autoplot() +
      labs(title = paste(title, "\n", char_startDate, "to", char_endDate))
    
    # Convert the ggplot object to a plotly object
    ggplotly(plot_stl) %>%
      layout(plot_bgcolor="#edf2f7")
  })
  # Time Series: Forecasting ----
  
  ## 1.1 Dynamic UI for ForecastTS_dynamic_time_resolution
  output$ForecastTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ForecastTS_selected_var)) {
      radioButtons("ForecastTS_time_resolution", label = "Select time resolution", c("Week"))
    } else if (grepl("Temperature", input$ForecastTS_selected_var)) {
      radioButtons("ForecastTS_time_resolution", label = "Select time resolution", c("Day" ,"Week"))
    }
  })
  
  output$ForecastTS_startDate_ui <- renderUI({
    if(input$ForecastTS_time_resolution == "Day"){
      dateInput("ForecastTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-11-30")
    } else if (input$ForecastTS_time_resolution == "Week"){
      dateInput("ForecastTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-06-25")
    }
    
  })
  ### Instructions
  output$ForecastTS_Date_Instructions <- renderUI({
    
    if(input$ForecastTS_time_resolution == "Day"){
      HTML("<em>Please select a period of at least 30 days for plotting. </em>")
    } else if (input$ForecastTS_time_resolution == "Week"){
      HTML("<em>Please select a period of at least 26 weeks for plotting. </em>")    }
    
  })
  # observe({
  #   # This will now only react to changes in 'input$ForecastTS_dynamic_time_resolution'
  #   if(input$ForecastTS_dynamic_time_resolution == "Week"){
  #     updateDateInput(session, "ForecastTS_startDate", max = "2023-06-01")
  #   }
  # })
  
  ## 1.3 Dynamic UI for ForecastTS_dynamic_chooseautoSTL
  output$ForecastTS_dynamic_chooseautoSTL <- renderUI({
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))
    ) {radioButtons("ForecastTS_chooseautoSTL", label = "Use Auto STL?" ,c("Yes", "No"))
    }
  })
  
  ## 1.3 Dynamic UI for ForecastTS_dynamic_model_parameters
  output$ForecastTS_dynamic_model_parameters <- renderUI({
    # Check if any of the STL options are selected
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))) {
      if(input$ForecastTS_chooseautoSTL == "No"
      ){
        list(sliderInput("ForecastTS_TrendWindow", "Trend Window", min = 1, max = 365, value = 20),
             sliderInput("ForecastTS_SeasonWindow", "Season Window", min = 1, max = 365, value = 20))
      } 
    }
  })
  ## 1.4 Dynamic UI for ForecastTS_dynamic_forecast_period
  output$ForecastTS_dynamic_forecast_period <- renderUI({
    end_date <- as.Date("2023-12-31")
    
    if (input$ForecastTS_time_resolution == "Day"){
      total_days <- as.numeric(difftime(end_date, input$ForecastTS_startDate, units = "days"))
      # Calculate maximum future forecasting horizon
      max_forecast_period <- floor(total_days / 12)
      sliderInput("ForecastTS_forecast_period", "Select Forecast Period (days)", min = 1, max = max(max_forecast_period, 1), value = min(10, max_forecast_period), step = 1)
    } else if (input$ForecastTS_time_resolution == "Week"){
      total_weeks <- as.numeric(difftime(end_date, input$ForecastTS_startDate, units = "weeks"))
      max_forecast_period <- floor(total_weeks / 12)
      sliderInput("ForecastTS_forecast_period", "Select Forecast Period (weeks)", min = 1, max = max(max_forecast_period, 1), value = min(10, max_forecast_period), step = 1)
    }
    
  })
  
  
  ## 2. Prepare data and create eventReactive object that updates only when the Build Model button is clicked
  model_building <- eventReactive(input$ForecastTS_build_model, {
    req(length(input$ForecastTS_selected_models) > 0)  # Ensure models are selected
    
    # Proceed with model fitting and forecasting logic
    result <- TS_prepareVariableData(input$ForecastTS_selected_var,as.character(input$ForecastTS_startDate),"2023-12-31", input$ForecastTS_selected_station,input$ForecastTS_time_resolution, weather_tsbl)
    
    variable_data <- result$variable_data
    title <- result$title
    
    split_point <- nrow(variable_data) * input$ForecastTS_train_test_split
    train_data <- variable_data %>% slice(1:floor(split_point))
    test_data <- variable_data %>% slice((floor(split_point) + 1):n())
    
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))){
      if(input$ForecastTS_chooseautoSTL == "No") {
        trend_window <- input$ForecastTS_TrendWindow
        season_window <- input$ForecastTS_SeasonWindow
      } else { 
        trend_window <- NULL
        season_window <- NULL
      }
    }
    
    ForecastTS_model_list <- list(
      STL_Naive = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), NAIVE(season_adjust)),
      STL_ARIMA = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ARIMA(season_adjust)),
      STL_ETS = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ETS(season_adjust ~ season("N"))),
      AUTO_ARIMA = ARIMA(ValueToPlot),
      AUTO_Prophet = prophet(ValueToPlot),
      AUTO_ETS = ETS(ValueToPlot)
    )
    
    
    selected_models <- ForecastTS_model_list[names(ForecastTS_model_list) %in% input$ForecastTS_selected_models]
    train_fit <- model(train_data, !!!selected_models) # Fit models to the train data
    forecast_horizon <- nrow(test_data) # Set forecasting horizon to length of test data
    forecasts <- forecast(train_fit, h = forecast_horizon) 
    
    selected_var <- input$ForecastTS_selected_var
    var_title <- if (grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {"Temperature (°C)"}
    
    # Return data to be used for plotting for Forecast validation
    list(variable_data = variable_data, train_fit = train_fit, forecasts = forecasts, test_data = test_data, train_data = train_data, title = title, var_title = var_title, selected_var = selected_var)
  })
  
  ## 3. Model Calibration tab
  ### Forecast Validation Plot
  output$ForecastTS_forecast_validation_plot <- renderPlotly({
    # Extract the result from the eventReactive object
    result <- model_building()
    # Extract data and variables from reactive expression
    variable_data <- result$variable_data
    title <- result$title
    train_data <- result$train_data
    test_data <- result$test_data
    forecasts <- result$forecasts
    selected_var <-result$selected_var
    var_title <- result$var_title
    
    plot <- autoplot(variable_data, ValueToPlot) + 
      autolayer(forecasts, level = NULL) + 
      labs(title = paste("Forecast Validation", title),  y = var_title) + 
      theme_minimal()
    
    p <- ggplotly(plot,
                  tooltip = c("x", "y", ".model"))
    
    
    return(p)
    
  })
  ### Residual Plot
  output$ForecastTS_residual_plot <- renderPlotly({
    # Extract the result from the eventReactive object
    result <- model_building()
    # Extract data and variables from reactive expression
    variable_data <- result$variable_data
    train_fit <- result$train_fit
    
    residual <- train_fit %>% augment()
    
    a <- autoplot(residual, .innov) +
      labs(title = "Residual Plot") +
      theme_minimal() 
    
    a_plotly <- ggplotly(a) %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.4, xanchor = "center", yanchor = "top"),
             margin = list(b = 80))
    return(a_plotly)
  })
  
  ### Data Table
  output$ForecastTS_buildModel_DataTable <- DT::renderDataTable({
    result <- model_building()
    test_data <- result$test_data
    forecasts <- result$forecasts
    
    accuracy_metrics <- accuracy(forecasts, test_data) %>%
      arrange(.model) %>%
      select(.model, .type, RMSE, MAE, MAPE) %>%
      mutate(across(c(RMSE, MAE, MAPE), round, 2))
    
    datatable(accuracy_metrics, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              options = list(pageLength = 10,scrollX=T))
    
  })
  
  ## 4. Prepare data and create eventReactive object that updates only when the Forecast button is clicked
  future_forecast <- eventReactive(input$ForecastTS_future_forecast, {
    req(length(input$ForecastTS_selected_models) > 0)
    
    
    # Proceed with model fitting and forecasting logic
    result <- TS_prepareVariableData(input$ForecastTS_selected_var,as.character(input$ForecastTS_startDate),"2023-12-31", input$ForecastTS_selected_station,input$ForecastTS_time_resolution, weather_tsbl)
    variable_data <- result$variable_data
    title <- result$title
    selected_var <- input$ForecastTS_selected_var
    var_title <- if (grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {"Temperature (°C)"}
    
    
    if (!is.null(input$ForecastTS_selected_models) &&
        any(grepl("STL", input$ForecastTS_selected_models))){
      if(input$ForecastTS_chooseautoSTL == "No") {
        trend_window <- input$ForecastTS_TrendWindow
        season_window <- input$ForecastTS_SeasonWindow
      } else { 
        trend_window <- NULL
        season_window <- NULL
      }
    }
    ForecastTS_model_list <- list(
      STL_Naive = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), NAIVE(season_adjust)),
      STL_ARIMA = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ARIMA(season_adjust)),
      STL_ETS = decomposition_model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window)), ETS(season_adjust ~ season("N"))),
      AUTO_ARIMA = ARIMA(ValueToPlot),
      AUTO_Prophet = prophet(ValueToPlot),
      AUTO_ETS = ETS(ValueToPlot)
    )
    
    selected_models <- ForecastTS_model_list[names(ForecastTS_model_list) %in% input$ForecastTS_selected_models]
    full_fit <- model(variable_data, !!!selected_models) # Fit models to the full dataset 
    future_horizon <- input$ForecastTS_forecast_period
    full_forecast <- forecast(full_fit, h = future_horizon)
    
    full_forecast_df <- as_tibble(full_forecast)
    
    full_forecast_df <- full_forecast_df %>%
      mutate(.mean = round(.mean, 2)) %>%
      mutate(n=n(), sd=sd(.mean)) %>%
      mutate(se=sd/sqrt(n-1)) %>%
      mutate(Lower = round(.mean - (1.96 * se), 2),
             Upper = round(.mean + (1.96 * se), 2)) 
    
    if (input$ForecastTS_time_resolution == "Week"){
      full_forecast_df <- full_forecast_df  %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    }
    
    # Initialize an empty plotly object
    p <- plot_ly()
    
    # Unique models for iteration and color assignment
    unique_models <- unique(full_forecast_df$.model)
    colors <- RColorBrewer::brewer.pal(n = length(unique_models), name = "Set1")
    
    # Loop through each model to add to the plot
    for (i in seq_along(unique_models)) {
      model_name <- unique_models[i]
      
      # Filter data for the current model
      model_data <- filter(full_forecast_df, .model == model_name)
      
      # Define the custom hovertemplate for lines
      if (input$ForecastTS_time_resolution == "Day"){
        hovertemplate_line <- paste(
          "Date: %{x}<br>",
          var_title, ": %{y:.2f}<br>",
          "Model: ", model_name, "<br>",
          "95% CI: [%{customdata[0]:.2f}, %{customdata[1]:.2f}]<extra></extra>"
        )
      } else if(input$ForecastTS_time_resolution == "Week") {
        hovertemplate_line <- paste(
          "Week starting: %{x}<br>",
          var_title, ": %{y:.2f}<br>",
          "Model: ", model_name, "<br>",
          "95% CI: [%{customdata[0]:.2f}, %{customdata[1]:.2f}]<extra></extra>"
        )
      }
      
      # Custom data for the line (lower and upper CI values)
      custom_data <- mapply(function(lower, upper) list(lower, upper), model_data$Lower, model_data$Upper, SIMPLIFY = FALSE)
      
      # Add forecast line with custom tooltip and legend grouping
      p <- add_lines(p, data = model_data, x = ~Date, y = ~`.mean`, name = model_name,
                     line = list(color = colors[i]), hovertemplate = hovertemplate_line,
                     customdata = custom_data, legendgroup = model_name)
      
      # Add confidence interval ribbon with legend grouping, but no separate legend entry
      p <- add_ribbons(p, data = model_data, x = ~Date, ymin = ~Lower, ymax = ~Upper,
                       fillcolor = scales::alpha(colors[i], 0.2), line = list(color = "transparent"),
                       legendgroup = model_name, showlegend = FALSE, hoverinfo = "skip")
    }
    
    
    
    # Customize layout
    p <- p %>% layout(title = paste("Future Forecast Plot with 95% CI for", title),
                      xaxis = list(title = "Date"),
                      yaxis = list(title = var_title),
                      legend = list(title = list(text = 'Model')),
                      hovermode = 'closest')
    
    
    # Return data to be used for plotting for Future Forecast
    list(p = p, full_forecast_df= full_forecast_df)
    
  })
  
  ## 5. Forecast Result tab 
  
  ### Future Forecast Plot
  output$ForecastTS_future_forecast_plot <- renderPlotly({
    
    result <- future_forecast()
    p<-result$p
    p
    
    
  })
  
  ### Data Table
  output$ForecastTS_future_forecast_DataTable <- DT::renderDataTable({
    result <- future_forecast()
    
    full_forecast_df <- result$full_forecast_df
    
    col<- full_forecast_df %>% 
      select(.model, Date, .mean) %>% 
      rename(`Forecasted Value` = .mean) %>%
      mutate(`Forecasted Value` = round(`Forecasted Value`, 2),
             .model = as.factor(.model))
    
    datatable(col, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              filter = 'top',
              options = list(pageLength = 10,scrollX=T))
  })
  
  
  # Geospatial: Map ----
  ## 1. Dynamic UI: selected time_resolution
  output$GS_dynamic_time_resolution <- renderUI({
    if (input$GS_time_resolution == "Day") {
      airDatepickerInput("GS_selected_date", label = "Select date", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM-dd")
    } else if (input$GS_time_resolution == "Month") {
      airMonthpickerInput("GS_selected_date", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$GS_time_resolution == "Year") {
      airYearpickerInput("GS_selected_date", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  ## 2.eventReactive
  GS_tmap_event <- eventReactive(input$GS_updatetmap, {
    
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    main_title <- paste(results$main_title, "across 11 stations in Singapore")
    
    list(variable_data = results$variable_data, variable_data_sf = results$variable_data_sf, legend_title= results$legend_title, main_title = main_title, palette= results$palette, var_title = results$var_title)
  })
  
  ## 3. Output plot
  output$GS_tmap <- renderTmap({
    
    GS_reactiveDataTmap <- GS_tmap_event()
    
    dynamicPopupVars <- setNames(list(GS_reactiveDataTmap$legend_title), GS_reactiveDataTmap$legend_title)
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    tm <- tm_shape(mpsz2019) +
      tm_borders() +
      tm_shape(GS_reactiveDataTmap$variable_data_sf) +
      tm_dots(col = 'ValueToPlot', popup.vars = dynamicPopupVars,palette = GS_reactiveDataTmap$palette, size = 0.5, scale = 0.5, title = GS_reactiveDataTmap$legend_title)  +
      tm_layout(title = GS_reactiveDataTmap$main_title) +
      tm_view(set.view = c(lon = 103.8198, lat = 1.3521, zoom = 11)) # Centered on Singapore with an appropriate zoom level
    
    tm
  })
  ### Plot title
  output$GS_tmap_title <- renderUI({
    title_text <- GS_tmap_event()$main_title
    tags$h4(title_text)
  })
  
  ### Data Table
  output$GS_tmap_DataTable <- DT::renderDataTable({ 
    # DT::datatable(head(mtcars), options = list(pageLength = 5))
    GS_reactiveDataTmap <- GS_tmap_event()
    
    variable_data <- GS_reactiveDataTmap$variable_data %>% select(-ValueToPlot)
    
    variable_data$Station <-  as.factor(variable_data$Station)
    
    datatable(variable_data, 
              class= "hover",
              rownames = FALSE,
              width="100%", 
              filter = 'top',
              options = list(pageLength = 10,scrollX=T))
  })
  
  
  
  # Geospatial: IDW ----
  
  ## 1. Dynamic UI: IDW Parameters
  ### Removed
  
  ## 2. eventReactive
  GS_IDW_event <- eventReactive(input$GS_updateIDW, {
    
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    raster_layer <- GS_rasterlayer(input$GS_IDW_res)
    variable_data <-results$variable_data
    variable_data_sf <- results$variable_data_sf
    main_title <- paste(results$main_title, "in Singapore")
    legend_title <- results$legend_title
    palette<- results$palette
    grid <- raster_layer$grid
    coop <- raster_layer$coop
    
    # Set nmax value
    nmax = input$GS_IDW_nmax
    
    # Create gstat object
    res <- gstat(formula = ValueToPlot ~ 1,locations = variable_data_sf, set = list(idp = 0), nmax = nmax)
    # Predict values
    resp <- predict(res,coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    pred <- rasterize(resp, grid, field="pred", fun="mean")
    
    list(variable_data = results$variable_data, variable_data_sf = results$variable_data_sf, legend_title= results$legend_title, main_title = main_title, palette= results$palette, var_title = results$var_title, pred = pred)
  })
  
  
  # 3. Output plot
  output$GS_IDW_map <- renderPlot({
    
    GS_reactiveDataIDW <- GS_IDW_event()
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataIDW$pred) +
      tm_raster(title = GS_reactiveDataIDW$legend_title, alpha = 0.6, palette = GS_reactiveDataIDW$palette) +
      tm_layout(main.title = GS_reactiveDataIDW$main_title, main.title.position = "center", main.title.size = 1.2, legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
  })
  
  # Geospatial: OK ----
  ## 1. Dynamic UI: OK Parameters
  
  output$GS_OK_range_reactiveUI <-  renderUI({
    if (input$GS_OK_model == "Pow") {
      sliderInput("GS_OK_range", "range", min = 0, max = 2, value = 1, step = 0.5)
    } else
    {sliderInput("GS_OK_range", "range", min = 2000, max = 10000, value = 5000, step = 200)}
  })
  
  
  ## 2. eventReactive
  GS_OK_event <- eventReactive(input$GS_updateOK, {
    
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    raster_layer <- GS_rasterlayer(input$GS_OK_res)
    variable_data <-results$variable_data
    variable_data_sf <- results$variable_data_sf
    main_title <- paste(results$main_title, "in Singapore")
    legend_title <- results$legend_title
    palette<- results$palette
    
    grid <- raster_layer$grid
    coop <- raster_layer$coop
    
    # Generate Experimental Variogram
    v <- variogram(ValueToPlot ~ 1,
                   data = variable_data_sf)
    
    # Fit Variogram
    fv <- fit.variogram(object = v,
                        model = vgm(psill = input$GS_OK_psill, model = input$GS_OK_model, range = input$GS_OK_range, nugget = input$GS_OK_nugget))
    
    # perform spatial interpolation
    k <- gstat(formula = ValueToPlot ~ 1,
               data = variable_data_sf,
               model = fv)
    
    # estimate the unknown grids
    resp <- predict(k,coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    
    # create a raster surface data object
    kpred <- rasterize(resp, grid,
                       field = "pred")
    
    # Extracting the variance
    resp$variance <- resp$var1.var
    
    # Create a raster surface data object for variance
    kvar <- rasterize(resp, grid, field = "variance")
    
    list(variable_data = results$variable_data, variable_data_sf = results$variable_data_sf, legend_title= results$legend_title, main_title = main_title, palette= results$palette, var_title = results$var_title, 
         kpred = kpred, kvar = kvar, v = v, fv = fv, UI_title = "Result of Interpolation")
  })
  
  # 3. Output plots
  output$GS_OK_variogram <- renderPlot({
    
    GS_reactiveDataOK <- GS_OK_event()
    
    plot(GS_reactiveDataOK$v, cex = 1.5, main = "Experimental Variogram")
    
  })
  output$GS_OK_fitted_variogram <- renderPlot({
    
    GS_reactiveDataOK <- GS_OK_event()
    
    plot(GS_reactiveDataOK$v, GS_reactiveDataOK$fv, cex = 1.5, main = "Fitted Variogram")
    
  })
  output$GS_OK_map <- renderPlot({
    GS_reactiveDataOK <- GS_OK_event()
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataOK$kpred) + 
      tm_raster(title = GS_reactiveDataOK$legend_title, alpha = 0.6, palette = GS_reactiveDataOK$palette) +
      tm_layout(main.title = GS_reactiveDataOK$main_title, main.title.position = "center", main.title.size = 1.2, 
                legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
  })
  output$GS_OK_prediction_variance <- renderPlot({
    GS_reactiveDataOK <- GS_OK_event()
    variance_title <- "Prediction Variance"
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataOK$kvar) + 
      tm_raster(title = variance_title, alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = "Kriging Prediction Variance", main.title.position = "center", main.title.size = 1.2, legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
    
  })
  
  
  output$GS_OK_result_title <- renderUI({
    UI_title <- GS_OK_event()$UI_title
    tags$h4(UI_title)
  })
}
# Section 9: Run the application ---- 
shinyApp(ui, server)