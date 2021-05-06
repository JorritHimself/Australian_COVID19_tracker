##### clear env ####
rm(list = ls())

# Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

# ##################### Fetch updated data ##########################
# #### Get data from sources:
# # Confirmed cases: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
# # Deaths: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv
# # Recovered: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv
# confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
# deaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
# recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))
# #### Prep data
# # confirmed cases
# confirmed <- confirmed %>%
#   filter(Country.Region=="Australia") %>%
#   filter(Province.State!="From Diamond Princess") %>%
#   select (-c(Country.Region, Lat, Long))%>%
#   rename(State = Province.State)
# confirmed <- gather(confirmed, key = "date", value = "confirmed", -"State")
# # Deaths
# deaths <- deaths %>%
#   filter(Country.Region=="Australia") %>%
#   filter(Province.State!="From Diamond Princess") %>%
#   select (-c(Country.Region, Lat, Long))%>%
#   rename(State = Province.State)
# deaths <- gather(deaths, key = "date", value = "deaths", -"State")
# # Deaths
# recovered <- recovered %>%
#   filter(Country.Region=="Australia") %>%
#   filter(Province.State!="From Diamond Princess") %>%
#   select (-c(Country.Region, Lat, Long))%>%
#   rename(State = Province.State)
# recovered <- gather(recovered, key = "date", value = "recovered", -"State")
# # Put it together
# aus.corona <- left_join(confirmed, deaths, by = c("State", "date"))
# aus.corona <- left_join(aus.corona, recovered, by = c("State", "date"))
# # Fix State factor levels mess
# aus.corona$State <- as.character(aus.corona$State)
# aus.corona$State <- as.factor(aus.corona$State)
# # fix datemess
# aus.corona$date <- gsub("X", "0", aus.corona$date)
# aus.corona$date <- as.Date(aus.corona$date, "%m.%d.%y")

# Save and use for now
aus.corona <- read_csv("C:/Users/jorri/OneDrive/Work/shiny corona aus/auscoronatemp.csv")
aus.corona.all <- aus.corona%>%
  group_by(date)%>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered))  
aus.corona.vic <- aus.corona%>%filter(aus.corona$State=="Victoria")
aus.corona.qld <- aus.corona%>%filter(aus.corona$State=="Queensland")
aus.corona.nsw <- aus.corona%>%filter(aus.corona$State=="New South Wales")
aus.corona.saus <- aus.corona%>%filter(aus.corona$State=="South Australia")
aus.corona.waus <- aus.corona%>%filter(aus.corona$State=="Western Australia")
aus.corona.tas <- aus.corona%>%filter(aus.corona$State=="Tasmania")
aus.corona.not <- aus.corona%>%filter(aus.corona$State=="Northern Territory")
aus.corona.act <- aus.corona%>%filter(aus.corona$State=="Australian Capital Territory")

# Get min and max dates in the full sample
date.max <- max(aus.corona$date, na.rm = TRUE)
date.min <- min(aus.corona$date, na.rm = TRUE)

#######################
# Define UI
ui <- pageWithSidebar(
  # Application title
  headerPanel("COVID-19 cases in Australia"),
  # Sidebar with a slider input
  sidebarPanel(
    selectInput("Status", "Status:", 
                c("Confirmed" = "confirmed",
                  "Deaths" = "deaths",
                  "Recovered" = "recovered")),
    sliderInput("range",
                "Show date range:",
                min = date.min,
                max = date.max,
                value = c(date.min, date.max))),
    # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot.all")
  )
)




# Define server logic to make plots  ----
server = function(input, output) {
  # Get values from slider 
  output$range <- renderPrint({input$slider})
  
  output$plot.all = renderPlot({
    ggplot(aus.corona.all) +
      geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) + 
      geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) + 
      geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) + 
      labs (x = "Date", y = "Cumulative cases", title = "National")
  })  

  
}

shinyApp(ui = ui, server)
