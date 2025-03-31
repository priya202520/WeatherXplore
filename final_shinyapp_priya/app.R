library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# Load and preprocess data
weather <- read_csv("data/weather_data_cleaned.csv") %>%
  mutate(
    Date = ymd(Date),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    TempRange = `Maximum Temperature (°C)` - `Minimum Temperature (°C)`
  )

# Define UI
ui <- fluidPage(
  titlePanel("WeatherXplore - Exploratory & Confirmatory Data Analysis"),
  tabsetPanel(
    tabPanel("Exploratory Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("eda_scenario", "Select EDA Scenario:",
                             choices = c("Monthly Avg Temp & Rainfall by Year",
                                         "Temperature Range by Station",
                                         "Top 10 Rainiest Days Each Year")),
                 
                 conditionalPanel(condition = "input.eda_scenario == 'Monthly Avg Temp & Rainfall by Year'",
                                  sliderInput("year_range", "Select Year Range:",
                                              min = min(weather$Year), max = max(weather$Year),
                                              value = c(min(weather$Year), max(weather$Year)), sep = ""),
                                  sliderInput("month_range", "Select Months:",
                                              min = 1, max = 12, value = c(1, 12), step = 1),
                                  selectInput("station_eda", "Select Station(s):",
                                              choices = unique(weather$Station),
                                              selected = unique(weather$Station)[1], multiple = TRUE)
                 ),
                 
                 conditionalPanel(condition = "input.eda_scenario == 'Temperature Range by Station'",
                                  selectInput("region_input", "Select Region:",
                                              choices = unique(weather$Region), selected = unique(weather$Region), multiple = TRUE),
                                  selectInput("station_temp_range", "Select Station(s):",
                                              choices = unique(weather$Station), selected = unique(weather$Station), multiple = TRUE),
                                  sliderInput("year_temp_range", "Select Year Range:",
                                              min = min(weather$Year), max = max(weather$Year),
                                              value = c(min(weather$Year), max(weather$Year))),
                                  sliderInput("month_temp_range", "Select Months:",
                                              min = 1, max = 12, value = c(1, 12))
                 ),
                 
                 conditionalPanel(condition = "input.eda_scenario == 'Top 10 Rainiest Days Each Year'",
                                  selectInput("year_rainiest", "Select Year:",
                                              choices = unique(weather$Year), selected = min(weather$Year))
                 )
               ),
               mainPanel(
                 plotlyOutput("eda_plot")
               )
             )
    ),
    
    tabPanel("Confirmatory Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cda_variable", "Choose Variable:",
                             choices = c("Mean Temperature (°C)", "Maximum Temperature (°C)",
                                         "Minimum Temperature (°C)", "Daily Rainfall Total (mm)")),
                 radioButtons("compare_by", "Compare Across:",
                              choices = c("Stations", "Years"), selected = "Stations"),
                 selectInput("cda_stations", "Select Stations:", choices = unique(weather$Station),
                             selected = unique(weather$Station)[1], multiple = TRUE),
                 selectInput("cda_year", "Select Year:", choices = unique(weather$Year)),
                 selectInput("cda_test", "Statistical Approach:", choices = c("ANOVA", "t-test")),
                 actionButton("update_cda", "Update Plot")
               ),
               mainPanel(
                 plotOutput("cda_plot"),
                 verbatimTextOutput("cda_note")
               )
             )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  output$eda_plot <- renderPlotly({
    if (input$eda_scenario == "Monthly Avg Temp & Rainfall by Year") {
      weather_filtered <- weather %>%
        filter(Year >= input$year_range[1], Year <= input$year_range[2],
               month(Date) >= input$month_range[1], month(Date) <= input$month_range[2],
               Station %in% input$station_eda) %>%
        group_by(Year, Month) %>%
        summarise(
          AvgTemp = mean(`Mean Temperature (°C)`, na.rm = TRUE),
          TotalRain = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(cols = c(AvgTemp, TotalRain), names_to = "Variable", values_to = "Value")
      
      p <- ggplot(weather_filtered, aes(x = Month, y = Value, color = as.factor(Year), group = Year)) +
        geom_line(linetype = "dashed") +
        facet_wrap(~Variable, scales = "free_y") +
        labs(title = "Monthly Temperature and Rainfall by Year") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    }
    else if (input$eda_scenario == "Temperature Range by Station") {
      weather_filtered <- weather %>%
        filter(Region %in% input$region_input,
               Station %in% input$station_temp_range,
               Year >= input$year_temp_range[1], Year <= input$year_temp_range[2],
               month(Date) >= input$month_temp_range[1], month(Date) <= input$month_temp_range[2])
      
      p <- ggplot(weather_filtered, aes(x = Date, y = TempRange, color = Region)) +
        geom_line(alpha = 0.5) +
        labs(title = "Temperature Range for Selected Stations", y = "Temp Range (°C)") +
        theme_minimal()
      ggplotly(p)
    }
    else if (input$eda_scenario == "Top 10 Rainiest Days Each Year") {
      top_rain <- weather %>%
        filter(Year == input$year_rainiest) %>%
        top_n(10, `Daily Rainfall Total (mm)`) %>%
        arrange(desc(`Daily Rainfall Total (mm)`))
      
      p <- ggplot(top_rain, aes(x = reorder(Station, `Daily Rainfall Total (mm)`),
                                y = `Daily Rainfall Total (mm)`, fill = Station)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Top 10 Rainiest Days in", input$year_rainiest), y = "Rainfall (mm)", x = "Station") +
        theme_minimal()
      ggplotly(p)
    }
  })
  
  observeEvent(input$update_cda, {
    output$cda_plot <- renderPlot({
      req(input$cda_variable, input$compare_by)
      data_filtered <- weather %>%
        filter((if (input$compare_by == "Stations") Year == input$cda_year else Station %in% input$cda_stations))
      
      var_sym <- sym(input$cda_variable)
      
      if (input$compare_by == "Stations") {
        data_filtered %>%
          filter(Station %in% input$cda_stations) %>%
          ggplot(aes(x = Station, y = !!var_sym)) +
          geom_violin(fill = "skyblue", alpha = 0.7) +
          geom_boxplot(width = 0.1) +
          labs(title = paste(input$cda_variable, "by Stations"))
      } else {
        data_filtered %>%
          ggplot(aes(x = as.factor(Year), y = !!var_sym)) +
          geom_violin(fill = "orange", alpha = 0.7) +
          geom_boxplot(width = 0.1) +
          labs(title = paste(input$cda_variable, "by Years"))
      }
    })
    
    output$cda_note <- renderText({
      paste("Statistical test:", ifelse(input$cda_test == "ANOVA", "parametric", "non-parametric"),
            "\nCompared by:", tolower(input$compare_by))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
