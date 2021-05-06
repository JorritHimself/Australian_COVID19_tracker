library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

df <- data.frame(Date = c("30/09/2018", "30/06/2018", "31/03/2018", "31/12/2017", "30/09/2017", "30/06/2017",
                          "31/03/2017", "30/09/2018", "30/06/2018", "31/03/2018", "31/12/2017", "30/09/2017", "30/06/2017",
                          "31/03/2017"),
                 Stock = c(rep("AAA", 7), rep("BBB", 7)),
                 Value = c(5.1, 5.2, 5.6, 5.5, 5.6, 5.7, 5.6, 6.4, 6.9, 6.7, 7.2, 7.2, 7.2, 7.7))

df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
df$Stock <- as.character(df$Stock)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Stock Financials Trend"),
  
  # Sidebar with slider input to select date range
  sidebarLayout(
    sidebarPanel(
      selectInput("Stock_selector",
                  "Stock:",
                  c("AAA", "BBB")),
      
      # Add a Slider Input to select date range
      sliderInput("Date_range_selector", "Select Date Range",
                  min = 2017,
                  max = 2018,
                  value = c(2017, 2018))
    ),
    
    # Show a plot of the trend
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  filtered_df <- reactive({
    df %>%
      filter(Stock == input$Stock_selector & between(year(Date), input$Date_range_selector[1], input$Date_range_selector[2]))
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_df(), aes_string(x = "Date", y = "Value")) + geom_line() + geom_point() +
      labs(title = paste(input$Stock_selector, "Trend", sep = " "), y = "Value")
  })    
}

# Run the application 
shinyApp(ui = ui, server = server)

