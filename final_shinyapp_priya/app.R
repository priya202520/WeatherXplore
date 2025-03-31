# Full EDA + CDA Shiny App with Improved Violin Overlay and Interactive Plot

# Load necessary packages
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggstatsplot)
library(parameters)
library(nortest)

# Load and preprocess data
weather <- read_csv("data/weather_data_cleaned.csv") %>%
  rename(
    MaxTemp = `Maximum Temperature (°C)`,
    MinTemp = `Minimum Temperature (°C)`,
    MeanTemp = `Mean Temperature (°C)`,
    Rainfall = `Daily Rainfall Total (mm)`
  ) %>%
  mutate(
    Date = ymd(Date),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    MonthNum = month(Date),
    TempRange = MaxTemp - MinTemp
  )

# UI
ui <- fluidPage(
  titlePanel("WeatherXplore - EDA & CDA"),
  tabsetPanel(
    tabPanel("Exploratory Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("eda_scenario", "Select EDA Scenario:",
                             choices = c("Monthly Average Temperature & Rainfall by Year",
                                         "Temperature Range by Station",
                                         "Top 10 Rainiest Days Each Year")),
                 
                 conditionalPanel(
                   condition = "input.eda_scenario == 'Monthly Average Temperature & Rainfall by Year'",
                   sliderInput("year_range", "Year Range:",
                               min = min(weather$Year), max = max(weather$Year),
                               value = c(min(weather$Year), max(weather$Year)), step = 1, sep = ""),
                   sliderInput("month_range", "Month Range:",
                               min = 1, max = 12, value = c(1, 12), step = 1, sep = ""),
                   selectInput("station_eda", "Stations:",
                               choices = unique(weather$Station),
                               selected = unique(weather$Station)[1], multiple = TRUE),
                   actionButton("plot_eda", "Plot Graph")
                 ),
                 
                 conditionalPanel(
                   condition = "input.eda_scenario == 'Temperature Range by Station'",
                   selectInput("region_input", "Region:",
                               choices = unique(weather$Region), selected = unique(weather$Region), multiple = TRUE),
                   selectInput("station_temp_range", "Stations:",
                               choices = unique(weather$Station), selected = unique(weather$Station), multiple = TRUE),
                   sliderInput("year_temp_range", "Year Range:",
                               min = min(weather$Year), max = max(weather$Year), value = c(min(weather$Year), max(weather$Year)), sep = ""),
                   sliderInput("month_temp_range", "Month Range:",
                               min = 1, max = 12, value = c(1, 12), step = 1, sep = ""),
                   actionButton("plot_temp_range", "Plot Graph")
                 ),
                 
                 conditionalPanel(
                   condition = "input.eda_scenario == 'Top 10 Rainiest Days Each Year'",
                   selectInput("year_rainiest", "Year:",
                               choices = unique(weather$Year), selected = min(weather$Year)),
                   actionButton("plot_rainiest", "Plot Graph")
                 )
               ),
               mainPanel(plotlyOutput("eda_plot"))
             )
    ),
    
    tabPanel("Confirmatory Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cda_variable", "Choose Variable:",
                             choices = c("Mean Temperature (°C)" = "MeanTemp",
                                         "Maximum Temperature (°C)" = "MaxTemp",
                                         "Minimum Temperature (°C)" = "MinTemp",
                                         "Daily Rainfall Total (mm)" = "Rainfall")),
                 selectInput("cda_stations", "Stations:",
                             choices = unique(weather$Station), selected = unique(weather$Station)[1:2], multiple = TRUE),
                 selectInput("cda_years", "Select Year(s):",
                             choices = unique(weather$Year), selected = unique(weather$Year)[1:2], multiple = TRUE),
                 selectInput("stat_approach", "Statistical Approach:",
                             choices = c("parametric", "nonparametric", "robust", "bayes")),
                 selectInput("conf_level", "Confidence Level:",
                             choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95),
                 textInput("plot_title", "Plot Title", "Station-wise Comparison"),
                 actionButton("run_test", "Run Test")
               ),
               mainPanel(
                 plotlyOutput("cda_plot"),
                 verbatimTextOutput("test_output")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$run_test, {
    req(input$cda_variable, input$cda_stations, input$cda_years)
    
    var <- input$cda_variable
    conf_level <- as.numeric(input$conf_level)
    
    data_filtered <- weather %>%
      filter(Station %in% input$cda_stations, Year %in% input$cda_years) %>%
      select(Station, Year, value = .data[[var]])
    
    # Plot with interactive violin + scatter overlay
    output$cda_plot <- renderPlotly({
      p <- ggplot(data_filtered, aes(x = Station, y = value, fill = Station)) +
        geom_violin(alpha = 0.5, color = NA) +
        geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.6) +
        geom_jitter(width = 0.2, height = 0, alpha = 0.4, size = 1, color = "black") +
        labs(title = input$plot_title, x = "Station", y = var) +
        theme_minimal() +
        theme(legend.position = "none")
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    output$test_output <- renderPrint({
      if (input$stat_approach == "parametric") {
        if (length(unique(data_filtered$Station)) == 2) {
          print(t.test(value ~ Station, data = data_filtered))
        } else {
          print(summary(aov(value ~ Station, data = data_filtered)))
        }
      } else if (input$stat_approach == "nonparametric") {
        if (length(unique(data_filtered$Station)) == 2) {
          print(wilcox.test(value ~ Station, data = data_filtered))
        } else {
          print(kruskal.test(value ~ Station, data = data_filtered))
        }
      } else if (input$stat_approach == "robust") {
        print(oneway_test(value ~ Station, data = data_filtered))
      } else if (input$stat_approach == "bayes") {
        print(bayesfactor_parameters(t_test(value ~ Station, data = data_filtered)))
      }
    })
  })
  
  observeEvent(input$plot_eda, {
    output$eda_plot <- renderPlotly({
      if (input$eda_scenario == "Monthly Average Temperature & Rainfall by Year") {
        filtered <- weather %>%
          filter(Year >= input$year_range[1], Year <= input$year_range[2],
                 MonthNum >= input$month_range[1], MonthNum <= input$month_range[2],
                 Station %in% input$station_eda) %>%
          group_by(Year, Month) %>%
          summarise(AvgTemp = mean(MeanTemp, na.rm = TRUE),
                    TotalRain = sum(Rainfall, na.rm = TRUE), .groups = 'drop') %>%
          pivot_longer(cols = c(AvgTemp, TotalRain), names_to = "Variable", values_to = "Value")
        
        p <- ggplot(filtered, aes(x = Month, y = Value, color = factor(Year), group = Year)) +
          geom_line(linewidth = 1) +
          facet_wrap(~Variable, scales = "free_y") +
          theme_minimal() +
          labs(title = "Monthly Average Temperature and Total Rain", x = "Month", y = NULL, color = "Year")
        ggplotly(p)
        
      } else if (input$eda_scenario == "Temperature Range by Station") {
        filtered <- weather %>%
          filter(Region %in% input$region_input,
                 Station %in% input$station_temp_range,
                 Year >= input$year_temp_range[1], Year <= input$year_temp_range[2],
                 MonthNum >= input$month_temp_range[1], MonthNum <= input$month_temp_range[2])
        
        p <- ggplot(filtered, aes(x = Date, y = TempRange, color = Region)) +
          geom_line(alpha = 0.6) + theme_minimal()
        ggplotly(p)
        
      } else if (input$eda_scenario == "Top 10 Rainiest Days Each Year") {
        filtered <- weather %>%
          filter(Year == input$year_rainiest) %>%
          slice_max(Rainfall, n = 10, with_ties = FALSE) %>%
          arrange(desc(Rainfall))
        
        p <- ggplot(filtered, aes(x = reorder(Station, Rainfall), y = Rainfall, fill = Station)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() +
          labs(title = paste("Top 10 Rainiest Days in", input$year_rainiest), y = "Rainfall (mm)", x = "Station")
        ggplotly(p)
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)