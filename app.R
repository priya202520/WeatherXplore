library(shiny)
library(tidyverse)
library(lubridate)

# Load cleaned weather data
weather_data <- read_csv("data/weather_data_cleaned.csv")

# Convert date column to Date format
weather_data <- weather_data %>%
  mutate(Date = as.Date(Date))

# Define UI with tabs
ui <- navbarPage("WeatherXplore Dashboard",
                 
                 tabPanel("Home",
                          fluidPage(
                            titlePanel("Welcome to WeatherXplore!"),
                            br(),
                            p("This application provides interactive visualizations of weather trends over time."),
                            p("Navigate to the 'Weather Visualizer' tab to explore temperature, humidity, and more."),
                            br(),
                            img(src = "weather_banner.png", height = "300px")  # Optional: add an image to your www/ folder
                          )
                 ),
                 
                 tabPanel("Weather Visualizer",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("variable", 
                                          "Select weather variable to visualize:",
                                          choices = names(weather_data)[!(names(weather_data) %in% c("Date"))],
                                          selected = "Temperature"),
                              
                              dateRangeInput("date_range", 
                                             "Select date range:",
                                             start = min(weather_data$Date),
                                             end = max(weather_data$Date))
                            ),
                            
                            mainPanel(
                              plotOutput("timePlot")
                            )
                          )
                 )
                 
                 # You can add more tabPanel() calls for other submodules here
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    weather_data %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2])
  })
  
  output$timePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = .data[[input$variable]])) +
      geom_line(color = "steelblue", size = 1) +
      labs(title = paste("Time Series of", input$variable),
           x = "Date",
           y = input$variable) +
      theme_minimal()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)


