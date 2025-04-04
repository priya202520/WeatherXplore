# tsa_module.R (Time Series Analysis as a Shiny module)

# Load necessary packages
pacman::p_load(shiny, tidyverse, lubridate, tsibble, fable, feasts, fable.prophet, plotly, DT, zoo)

# Load and preprocess data
weather_data <- read_csv("data/weather_data_cleaned.csv")

weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date) %>%
  group_by_key() %>%
  fill_gaps(.full = TRUE) %>%
  mutate(
    `Mean Temperature (°C)` = zoo::na.approx(`Mean Temperature (°C)`, x = Date, na.rm = FALSE),
    `Daily Rainfall Total (mm)` = zoo::na.approx(`Daily Rainfall Total (mm)`, x = Date, na.rm = FALSE)
  ) %>%
  ungroup()

variables_select <- c(
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)"
)

# UI Module
tsaUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Time Series Visualization",
             fluidPage(
               fluidRow(
                 column(2,
                        wellPanel(
                          selectInput(ns("variable"), "Weather Variable:", choices = variables_select),
                          radioButtons(ns("resolution"), "Time Resolution:",
                                       choices = c("Daily" = "day", "Weekly" = "week")),
                          selectInput(ns("station"), "Select Station(s):",
                                      choices = c("All Stations", unique(weather_tsbl$Station)),
                                      selected = "All Stations", multiple = TRUE),
                          dateRangeInput(ns("daterange"), "Date Range:",
                                         start = min(weather_tsbl$Date), end = max(weather_tsbl$Date))
                        )
                 ),
                 column(10,
                        plotlyOutput(ns("ts_plot"), height = "500px")
                 )
               )
             )
    ),
    
    tabPanel("Time Series Decomposition",
             fluidPage(
               fluidRow(
                 column(2,
                        wellPanel(
                          selectInput(ns("decomp_station"), "Select Station:", choices = unique(weather_tsbl$Station)),
                          radioButtons(ns("decomp_resolution"), "Time Resolution:",
                                       choices = c("Daily" = "day", "Weekly" = "week")),
                          sliderInput(ns("acf_lag"), "ACF/PACF Lag:", min = 10, max = 100, value = 50),
                          actionButton(ns("run_decomp"), "Run Decomposition")
                        )
                 ),
                 column(10,
                        h4(textOutput(ns("decomp_title"))),
                        tabsetPanel(
                          tabPanel("ACF & PACF",
                                   fluidRow(
                                     column(6, plotOutput(ns("acf_plot"), height = "300px")),
                                     column(6, plotOutput(ns("pacf_plot"), height = "300px"))
                                   )
                          ),
                          tabPanel("STL Decomposition",
                                   plotOutput(ns("stl_plot"), height = "400px")
                          )
                        )
                 )
               )
             )
    ),
    
    tabPanel("Future Forecast (Refitted)",
             fluidPage(
               fluidRow(
                 column(2,
                        wellPanel(
                          selectInput(ns("full_var"), "Weather Variable:", choices = variables_select),
                          selectInput(ns("full_station"), "Select Station:", choices = unique(weather_tsbl$Station)),
                          radioButtons(ns("horizon_unit"), "Forecast Period Unit:",
                                       choices = c("Days" = "day", "Weeks" = "week")),
                          sliderInput(ns("full_horizon"), "Forecast Horizon:", min = 1, max = 90, value = 30),
                          checkboxGroupInput(ns("full_models"), "Forecasting Models:",
                                             choices = c("STL + Naive" = "STLNaive",
                                                         "STL + ARIMA" = "STLArima",
                                                         "STL + ETS" = "STLETS",
                                                         "Auto ARIMA" = "AUTOARIMA",
                                                         "Auto Prophet" = "AUTOprophet",
                                                         "Auto ETS" = "AUTOETS"),
                                             selected = c("AUTOARIMA", "AUTOETS")),
                          actionButton(ns("run_future"), "Generate Forecast")
                        )
                 ),
                 column(10,
                        uiOutput(ns("future_title")),
                        plotlyOutput(ns("future_forecast_plot"), height = "300px"),
                        DTOutput(ns("future_table"), height = "300px")
                 )
               )
             )
    )
  )
}

# Server Module
tsaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_data <- reactive({
      data <- weather_tsbl
      if (!("All Stations" %in% input$station)) {
        data <- data %>% filter(Station %in% input$station)
      }
      data <- data %>% filter(Date >= input$daterange[1], Date <= input$daterange[2])
      
      if (input$resolution == "week") {
        data %>%
          mutate(Period = floor_date(Date, "week")) %>%
          group_by(Station, Period) %>%
          summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
      } else {
        data %>% mutate(Period = Date, Value = .data[[input$variable]]) %>%
          select(Station, Period, Value)
      }
    })
    
    output$ts_plot <- renderPlotly({
      p <- ggplot(selected_data(), aes(x = Period, y = Value, color = Station)) +
        geom_line() +
        labs(title = paste("Time Series of", input$variable), x = "Date", y = input$variable) +
        theme_minimal()
      ggplotly(p)
    })
    
    observeEvent(input$run_decomp, {
      station <- input$decomp_station
      resolution <- input$decomp_resolution
      var <- "Mean Temperature (°C)"
      
      data_filtered <- weather_tsbl %>%
        filter(Station == station) %>%
        select(Date, Station, value = all_of(var))
      
      if (resolution == "week") {
        data_filtered <- data_filtered %>%
          index_by(Date = ~ floor_date(Date, "week")) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
      }
      
      tsbl <- as_tsibble(data_filtered, key = Station, index = Date)
      
      output$acf_plot <- renderPlot({
        tsbl %>%
          ACF(value, lag_max = input$acf_lag) %>%
          autoplot() +
          labs(title = "ACF", x = "Lag", y = "Correlation") +
          theme_minimal()
      })
      
      output$pacf_plot <- renderPlot({
        tsbl %>%
          PACF(value, lag_max = input$acf_lag) %>%
          autoplot() +
          labs(title = "PACF", x = "Lag", y = "Partial Correlation") +
          theme_minimal()
      })
      
      stl_fit <- tsbl %>% model(STL(value))
      
      output$stl_plot <- renderPlot({
        components(stl_fit) %>%
          autoplot() +
          labs(title = "STL Decomposition", x = "Date") +
          theme_minimal()
      })
      
      output$decomp_title <- renderText({
        paste("Time Series Decomposition for", station, "-", str_to_title(resolution))
      })
    })
    
    observeEvent(input$run_future, {
      req(input$full_models)
      
      raw_data <- weather_tsbl %>% filter(Station == input$full_station)
      
      if (input$full_var == "Mean Temperature (°C)" && input$horizon_unit == "week") {
        full_ts <- raw_data %>%
          mutate(Date = floor_date(Date, "week")) %>%
          group_by(Date) %>%
          summarise(value = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop")
      } else if (input$full_var == "Daily Rainfall Total (mm)" && input$horizon_unit == "week") {
        full_ts <- raw_data %>%
          mutate(Date = floor_date(Date, "week")) %>%
          group_by(Date) %>%
          summarise(value = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop")
      } else {
        full_ts <- raw_data %>%
          select(Date, value = all_of(input$full_var))
      }
      
      full_ts <- as_tsibble(full_ts, index = Date)
      
      models <- full_ts %>%
        model(
          STLNaive = decomposition_model(STL(value), NAIVE(season_adjust)),
          STLArima = decomposition_model(STL(value), ARIMA(season_adjust)),
          STLETS = decomposition_model(STL(value), ETS(season_adjust ~ season("N"))),
          AUTOARIMA = ARIMA(value),
          AUTOprophet = prophet(value),
          AUTOETS = ETS(value)
        ) %>%
        select(all_of(input$full_models))
      
      fc <- forecast(models, h = paste(input$full_horizon, input$horizon_unit))
      
      output$future_forecast_plot <- renderPlotly({
        autoplot(fc, level = 95) +
          labs(x = "Date", y = input$full_var) +
          theme_minimal() %>%
          ggplotly(tooltip = c("x", "y", ".model"))
      })
      
      output$future_table <- renderDT({
        fc_tbl <- as_tibble(fc)
        if (input$horizon_unit == "week") {
          fc_tbl <- fc_tbl %>%
            mutate(year_week = paste(year(Date), "w", sprintf("%02d", isoweek(Date)), sep = "")) %>%
            select(.model, year_week, .mean) %>%
            rename(Forecast = .mean)
        } else {
          fc_tbl <- fc_tbl %>%
            mutate(date_str = as.character(Date)) %>%
            select(.model, date_str, .mean) %>%
            rename(Forecast = .mean)
        }
        
        datatable(fc_tbl,
                  class = "hover",
                  rownames = FALSE,
                  filter = 'top',
                  options = list(pageLength = 6, scrollX = TRUE))
      })
      
      output$future_title <- renderUI({
        h4(paste("Future Forecast for", input$full_var, "at", input$full_station))
      })
    })
  })
}