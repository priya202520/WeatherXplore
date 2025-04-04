# geofacet.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(geofacet)
library(plotly)

Sys.setlocale("LC_TIME", "C")

# Load the data once at the top
weather_data <- read_csv("data/weather_data_cleaned.csv")
weather_data$Date <- as.Date(weather_data$Date)

# Generate month choices
month_seq <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2024-12-01"), by = "month")
month_choices <- format(month_seq, "%Y-%m")
names(month_choices) <- format(month_seq, "%b %Y")

# Custom grid
custom_grid <- data.frame(
  name = c("Admiralty", "Sembawang", "Seletar", "Pulau Ubin",
           "Ang Mo Kio", "Newton", "Tai Seng", "Paya Lebar",
           "Choa Chu Kang (South)", "Jurong (West)", "Clementi",
           "Jurong Island", "Tuas South", "Sentosa Island",
           "Pasir Panjang", "East Coast Parkway", "Changi"),
  code = c("Admiralty", "Sembawang", "Seletar", "Pulau Ubin",
           "Ang Mo Kio", "Newton", "Tai Seng", "Paya Lebar",
           "Choa Chu Kang (South)", "Jurong (West)", "Clementi",
           "Jurong Island", "Tuas South", "Sentosa Island",
           "Pasir Panjang", "East Coast Parkway", "Changi"),
  row = c(2, 1, 2, 2, 3, 3, 4, 3, 2, 3, 4, 5, 4, 5, 4, 4, 3),
  col = c(3, 3, 4, 5, 4, 3, 4, 5, 2, 2, 2, 2, 1, 3, 3, 5, 6),
  stringsAsFactors = FALSE
)

# UI Module
geofacetUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Geofacet Weather Visualization", style = "margin-top: 20px; text-align: left;"),
    tabsetPanel(
      tabPanel("Main",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              selectInput(ns("variable"), "Select Variable:",
                                          choices = c("Total Rainfall (mm)" = "Daily Rainfall Total (mm)",
                                                      "Mean Temperature (°C)" = "Mean Temperature (°C)",
                                                      "Maximum Temperature (°C)" = "Maximum Temperature (°C)",
                                                      "Minimum Temperature (°C)" = "Minimum Temperature (°C)")),
                              selectInput(ns("resolution"), "Select Time Resolution:",
                                          choices = c("Monthly", "Yearly")),
                              conditionalPanel(
                                condition = sprintf("input['%s'] == 'Monthly'", ns("resolution")),
                                selectInput(ns("start_month"), "Start Month:", choices = month_choices, selected = "2024-01"),
                                selectInput(ns("end_month"), "End Month:", choices = month_choices, selected = "2024-12")
                              ),
                              conditionalPanel(
                                condition = sprintf("input['%s'] == 'Yearly'", ns("resolution")),
                                sliderInput(ns("year_range"), "Select Year Range:",
                                            min = 2020, max = 2024, value = c(2022, 2024), step = 1, sep = "")
                              )
                 ),
                 mainPanel(plotlyOutput(ns("geofacetPlot"), height = "800px"))
               )
      ),
      tabPanel("Extreme Weather",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              selectInput(ns("extreme_var"), "Select Variable:",
                                          choices = c("Total Rainfall (mm)" = "Daily Rainfall Total (mm)",
                                                      "Maximum Temperature (°C)" = "Maximum Temperature (°C)",
                                                      "Minimum Temperature (°C)" = "Minimum Temperature (°C)")),
                              numericInput(ns("threshold"), "Threshold:", value = 30, step = 0.1),
                              radioButtons(ns("direction"), "Threshold Comparison:",
                                           choices = c("≥" = "greater", "≤" = "less"),
                                           selected = "greater"),
                              selectInput(ns("extreme_res"), "Time Resolution:", choices = c("Monthly", "Yearly")),
                              conditionalPanel(
                                condition = sprintf("input['%s'] == 'Monthly'", ns("extreme_res")),
                                selectInput(ns("ext_start"), "Start Month:", choices = month_choices, selected = "2024-01"),
                                selectInput(ns("ext_end"), "End Month:", choices = month_choices, selected = "2024-12")
                              ),
                              conditionalPanel(
                                condition = sprintf("input['%s'] == 'Yearly'", ns("extreme_res")),
                                sliderInput(ns("ext_years"), "Select Year Range:",
                                            min = 2020, max = 2024, value = c(2022, 2024), step = 1, sep = "")
                              )
                 ),
                 mainPanel(plotlyOutput(ns("extremePlot"), height = "800px"))
               )
      )
    )
  )
}

# Server Module
geofacetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$geofacetPlot <- renderPlotly({
      df <- weather_data %>%
        mutate(
          Year = year(Date),
          YearMonth = floor_date(Date, "month")
        )
      
      variable_label <- names(which(c("Daily Rainfall Total (mm)",
                                      "Mean Temperature (°C)",
                                      "Maximum Temperature (°C)",
                                      "Minimum Temperature (°C)") == input$variable))
      is_rainfall <- input$variable == "Daily Rainfall Total (mm)"
      
      if (input$resolution == "Yearly") {
        yrange <- input$year_range
        filtered_df <- df %>% filter(Year >= yrange[1], Year <= yrange[2])
        
        agg_df <- filtered_df %>%
          group_by(Station, Region, Year) %>%
          summarise(Value = if (is_rainfall) sum(.data[[input$variable]], na.rm = TRUE)
                    else if (input$variable == "Mean Temperature (°C)") mean(.data[[input$variable]], na.rm = TRUE)
                    else if (input$variable == "Maximum Temperature (°C)") max(.data[[input$variable]], na.rm = TRUE)
                    else min(.data[[input$variable]], na.rm = TRUE),
                    .groups = "drop")
        
        plt <- ggplot(agg_df, aes(x = Year, y = Value, fill = Region,
                                  text = paste0("Station: ", Station,
                                                "<br>Year: ", Year,
                                                "<br>", variable_label, ": ", round(Value, 2)))) +
          (if (is_rainfall) geom_col() else geom_line(aes(color = Region, group = Station), size = 1)) +
          facet_geo(~ Station, grid = custom_grid) +
          theme_minimal() +
          labs(title = paste("Yearly", variable_label, "by Station"),
               x = "Year", y = variable_label) +
          theme(strip.text = element_text(size = 9, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else {
        start_date <- as.Date(paste0(input$start_month, "-01"))
        end_date <- ceiling_date(as.Date(paste0(input$end_month, "-01")), "month") - 1
        
        filtered_df <- df %>% filter(YearMonth >= start_date & YearMonth <= end_date)
        
        agg_df <- filtered_df %>%
          group_by(Station, Region, YearMonth) %>%
          summarise(Value = if (is_rainfall) sum(.data[[input$variable]], na.rm = TRUE)
                    else if (input$variable == "Mean Temperature (°C)") mean(.data[[input$variable]], na.rm = TRUE)
                    else if (input$variable == "Maximum Temperature (°C)") max(.data[[input$variable]], na.rm = TRUE)
                    else min(.data[[input$variable]], na.rm = TRUE),
                    .groups = "drop")
        
        if (is_rainfall) {
          agg_df <- agg_df %>%
            mutate(MonthLabel = factor(format(YearMonth, "%b %Y"),
                                       levels = format(seq(start_date, end_date, by = "month"), "%b %Y")))
          
          plt <- ggplot(agg_df, aes(x = MonthLabel, y = Value, fill = Region,
                                    text = paste0("Station: ", Station,
                                                  "<br>Date: ", MonthLabel,
                                                  "<br>", variable_label, ": ", round(Value, 2)))) +
            geom_col() +
            facet_geo(~ Station, grid = custom_grid) +
            theme_minimal() +
            labs(title = paste("Monthly", variable_label, "by Station"),
                 x = "Month", y = variable_label) +
            theme(strip.text = element_text(size = 9, face = "bold"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5))
        } else {
          plt <- ggplot(agg_df, aes(x = YearMonth, y = Value, color = Region, group = Station,
                                    text = paste0("Station: ", Station,
                                                  "<br>Date: ", format(YearMonth, "%b %Y"),
                                                  "<br>", variable_label, ": ", round(Value, 2)))) +
            geom_line(size = 1) +
            scale_x_date(
              date_labels = "%b %Y",
              date_breaks = "1 month",
              limits = c(start_date, end_date)
            ) +
            facet_geo(~ Station, grid = custom_grid) +
            theme_minimal() +
            labs(title = paste("Monthly", variable_label, "by Station"),
                 x = "Month", y = variable_label) +
            theme(strip.text = element_text(size = 9, face = "bold"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5))
        }
      }
      
      ggplotly(plt, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white"))
    })
    
    output$extremePlot <- renderPlotly({
      df <- weather_data %>%
        mutate(
          Year = year(Date),
          Month = floor_date(Date, "month"),
          YearMonth = format(Month, "%b %Y")
        )
      
      var <- input$extreme_var
      threshold <- input$threshold
      direction <- input$direction
      label <- names(which(c("Daily Rainfall Total (mm)",
                             "Maximum Temperature (°C)",
                             "Minimum Temperature (°C)") == var))
      
      comparison <- if (direction == "greater") {
        function(x) x >= threshold
      } else {
        function(x) x <= threshold
      }
      
      if (input$extreme_res == "Yearly") {
        yrange <- input$ext_years
        filtered <- df %>% filter(Year >= yrange[1], Year <= yrange[2])
        
        extreme_df <- filtered %>%
          filter(comparison(.data[[var]])) %>%
          group_by(Station, Region, Year) %>%
          summarise(Days = n(), .groups = "drop")
        
        plt <- ggplot(extreme_df, aes(x = Year, y = Days, fill = Region)) +
          geom_col() +
          facet_geo(~ Station, grid = custom_grid) +
          theme_minimal() +
          labs(title = paste("Extreme Days per Year (", label, ifelse(direction == "greater", " ≥ ", " ≤ "), threshold, ")", sep = ""),
               x = "Year", y = "Number of Extreme Days") +
          theme(strip.text = element_text(size = 9, face = "bold"))
        
      } else {
        start_date <- as.Date(paste0(input$ext_start, "-01"))
        end_date <- ceiling_date(as.Date(paste0(input$ext_end, "-01")), "month") - 1
        
        filtered <- df %>% filter(Date >= start_date & Date <= end_date)
        
        extreme_df <- filtered %>%
          filter(comparison(.data[[var]])) %>%
          mutate(MonthLabel = factor(format(Month, "%b %Y"),
                                     levels = format(seq(start_date, end_date, by = "month"), "%b %Y"))) %>%
          group_by(Station, Region, MonthLabel) %>%
          summarise(Days = n(), .groups = "drop")
        
        plt <- ggplot(extreme_df, aes(x = MonthLabel, y = Days, fill = Region)) +
          geom_col() +
          facet_geo(~ Station, grid = custom_grid) +
          theme_minimal() +
          labs(title = paste("Extreme Days per Month (", label, ifelse(direction == "greater", " ≥ ", " ≤ "), threshold, ")", sep = ""),
               x = "Month", y = "Number of Extreme Days") +
          theme(strip.text = element_text(size = 9, face = "bold"),
                axis.text.x = element_text(angle = 90, vjust = 0.5))
      }
      
      ggplotly(plt)
    })
  })
}
