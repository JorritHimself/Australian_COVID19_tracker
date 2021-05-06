##### clear env ####
rm(list = ls())

# Load packages
library(shiny)
library(plotly)
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
# # Calculate national totals
# temp.sum.nat <- aus.corona%>%
#   group_by(date)%>%
#   summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered))  
# temp.sum.nat$State <- "National"
# aus.corona <- rbind(aus.corona, temp.sum.nat)


# Use prepped file for now
aus.corona <- read_csv("C:/Users/jorri/OneDrive/Work/shiny corona aus/auscoronatemp.csv")

# Get min and max dates in the full sample
date.max <- max(aus.corona$date, na.rm = TRUE)
date.min <- min(aus.corona$date, na.rm = TRUE)

# List of States to loop over
statelist <- c("National", "Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")


########################################
## Run in interactive R session
if (interactive()) {
  options(device.ask.default = FALSE)
  
  # Define UI
  ui <- fluidPage(
    #  title
    titlePanel("Australian COVID-19 cases"),
    sidebarLayout(
      #### Sidebar 
      sidebarPanel(
        checkboxGroupInput("showcasechoice", "Status:", 
                    choices = c("Confirmed" = "confirmed",
                      "Deaths" = "deaths",
                      "Recovered" = "recovered"),
                    selected = c("confirmed", "deaths", "recovered")
                    ),
        radioButtons("showserieschoice", "Type", 
                           choices = c("Cumulative cases",
                                       "Daily cases"),
                           selected = c("Cumulative cases")
        )
      ),
      ### main output panel
      mainPanel(
        plotOutput("plot1"),
        plotOutput("plot2")
      )
    )
  )
  # Server logic
  server <- function(input, output) {
    ###Check what boxes are ticked
    show.which.cases <- reactive({
      as.vector(input$showcasechoice)
    }) 
    ##### Use inputs to create different graphs
    output$plot.all = renderPlot({
    if(("confirmed" %in% show.which.cases()) & ("deaths" %in% show.which.cases()) & ("recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) +
          geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) + 
        geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }
    else if(("confirmed" %in% show.which.cases()) & ("deaths" %in% show.which.cases()))
      {
      ggplot(aus.corona.all) +
        geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) +
        geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) + 
        labs (x = "Date", y = "Cumulative cases", title = "National")
      }
    else if(("confirmed" %in% show.which.cases()) & ("recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) +
          geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) + 
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }  
    else if(("deaths" %in% show.which.cases()) & ("recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) +
          geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) + 
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }     
    else if(("confirmed" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      } 
      else if(("deaths" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      } 
    else if(("recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }   
    })
  }
  # Complete app with UI and server components
  shinyApp(ui, server)
}









