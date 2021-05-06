##### clear env ####
rm(list = ls())
# Load packages
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

aus.corona <- read_csv("C:/Users/jorri/OneDrive/Work/shiny corona aus/auscoronatemp.csv")
aus.corona.all <- aus.corona%>%
  filter(state == "National")
# Get min and max dates in the full sample
date.max <- max(aus.corona$date, na.rm = TRUE)
date.min <- min(aus.corona$date, na.rm = TRUE)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Australian COVID-19 cases"),
  # Sidebar with slider input to select date range
  sidebarLayout(
    sidebarPanel(
      # Add a Slider Input to select date range
      sliderInput("daterange", "Date range shown:",
                  min = date.min,
                  max = date.max,
                  value = c(date.min, date.max))
    ),
    
    # Show a plot of the trend
    mainPanel(
      plotOutput("plot.nat")
    )
  )
)

server <- function(input, output) {
  filtered_df <- reactive({
    aus.corona.all %>%
      filter(between(date, input$daterange[1], input$daterange[2]))
  })
  
  output$plot.nat <- renderPlot({
    ggplot(filtered_df(), aes(x = date))+
      geom_line(aes(y = confirmed.tot, colour = "Confirmed"))+
      geom_line(aes(y = deaths.tot, colour = "Deaths"))+
      geom_line(aes(y = recovered.tot, colour = "Recovered"))+
      geom_point(aes(y = confirmed.tot, colour = "Confirmed"))+
      geom_point(aes(y = deaths.tot, colour = "Deaths"))+
      geom_point(aes(y = recovered.tot, colour = "Recovered"))
  })    
}

# Works:
# output$plot.nat <- renderPlot({
#   ggplot(filtered_df(), aes(x = date, y = confirmed.tot, colour = "Confirmed")) + 
#     geom_line() + geom_point()
# })    
# }

# Run the application 
shinyApp(ui = ui, server = server)

# ggplot(test_data, aes(date)) + 
#   geom_line(aes(y = var0, colour = "var0")) + 
#   geom_line(aes(y = var1, colour = "var1"))