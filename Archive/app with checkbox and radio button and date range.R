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
#   rename(state = Province.State)
# confirmed <- gather(confirmed, key = "date", value = "confirmed", -"state")
# # Deaths
# deaths <- deaths %>%
#   filter(Country.Region=="Australia") %>%
#   filter(Province.State!="From Diamond Princess") %>%
#   select (-c(Country.Region, Lat, Long))%>%
#   rename(state = Province.State)
# deaths <- gather(deaths, key = "date", value = "deaths", -"state")
# # Deaths
# recovered <- recovered %>%
#   filter(Country.Region=="Australia") %>%
#   filter(Province.State!="From Diamond Princess") %>%
#   select (-c(Country.Region, Lat, Long))%>%
#   rename(state = Province.State)
# recovered <- gather(recovered, key = "date", value = "recovered", -"state")
# # Put it together
# aus.corona <- left_join(confirmed, deaths, by = c("state", "date"))
# aus.corona <- left_join(aus.corona, recovered, by = c("state", "date"))
# # Fix State factor levels mess
# aus.corona$state <- as.character(aus.corona$state)
# aus.corona$state <- as.factor(aus.corona$state)
# # fix datemess
# aus.corona$date <- gsub("X", "0", aus.corona$date)
# aus.corona$date <- as.Date(aus.corona$date, "%m.%d.%y")
# # Calculate national totals
# temp.sum.nat <- aus.corona%>%
#   group_by(date)%>%
#   summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered))
# temp.sum.nat$state <- "National"
# aus.corona <- rbind(aus.corona, temp.sum.nat)
# # Make total and daily
# aus.corona <- aus.corona%>%
#   rename(confirmed.tot = confirmed,
#          deaths.tot = deaths,
#          recovered.tot = recovered)
# # new cases
# aus.corona <- aus.corona%>%
#   group_by(state)%>%
#   mutate(confirmed.lag1 = lag(confirmed.tot, n = 1, order_by = date)) %>%
#   mutate(deaths.lag1 = lag(deaths.tot, n = 1, order_by = date)) %>%
#   mutate(recovered.lag1 = lag(recovered.tot, n = 1, order_by = date)) 
# aus.corona$confirmed.new <- aus.corona$confirmed.tot - aus.corona$confirmed.lag1
# aus.corona$deaths.new <- aus.corona$deaths.tot - aus.corona$deaths.lag1
# aus.corona$recovered.new <- aus.corona$recovered.tot - aus.corona$recovered.lag1





# Save and use for now
aus.corona <- read_csv("C:/Users/jorri/OneDrive/Work/shiny corona aus/auscoronatemp.csv")
aus.corona.all <- aus.corona%>%
  filter(state == "National")

# Get min and max dates in the full sample
date.max <- max(aus.corona$date, na.rm = TRUE)
date.min <- min(aus.corona$date, na.rm = TRUE)

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
                    choices = c("Confirmed", "Deaths", "Recovered"),
                    selected = c("Confirmed", "Deaths", "Recovered")),
        radioButtons("showserieschoice", "Type of cases:", 
                     choices = c("Cumulative cases",
                                 "Daily cases"),
                     selected = c("Cumulative cases")),
        sliderInput("daterange",
                    "Show date range:",
                    min = date.min,
                    max = date.max,
                    value = c(date.min, date.max))
      ),
      ### main output panel
      mainPanel(
        plotOutput("plot.all")
      )
    )
  )
  # Server logic
  server <- function(input, output) {
    ###Check what boxes are ticked
    show.which.cases <- reactive({
      as.vector(input$showcasechoice)
    }) 
    show.type.cases <- reactive({
      as.vector(input$showserieschoice)
    }) 
    ### get date ranges and update table
    min.date.selected <- renderText({
      # seq.int(from = input$range[1], to = input$range[2])
      input$daterange[1]
    })
    max.date.selected <- renderText({
      # seq.int(from = input$range[1], to = input$range[2])
      input$daterange[2]
    })
    ### Update table
    temptding <- aus.corona %>%
      filter(date>=min.date.selected)%>%
      filter(date<= max.date.selected)
    
    ##### Use inputs to create different graphs
    output$plot.all = renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases()))
      {
      ggplot(temptding) +
        geom_line(mapping = aes(x = date, y = confirmed.tot, colour = "Confirmed")) +
        geom_line(mapping = aes(x = date, y = deaths.tot, colour = "Deaths")) + 
        geom_line(mapping = aes(x = date, y = recovered.tot, colour = "Recovered")) + 
        labs (x = "Date", y = "Cumulative cases", title = "National")
      }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()))
      {
      ggplot(aus.corona.all) +
        geom_line(mapping = aes(x = date, y = confirmed.tot, colour = "Confirmed")) +
        geom_line(mapping = aes(x = date, y = deaths.tot, colour = "Deaths")) + 
        labs (x = "Date", y = "Cumulative cases", title = "National")
      }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = confirmed.tot, colour = "Confirmed")) +
          geom_line(mapping = aes(x = date, y = recovered.tot, colour = "Recovered")) + 
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }  
    else if(("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = deaths.tot, colour = "Deaths")) +
          geom_line(mapping = aes(x = date, y = recovered.tot, colour = "Recovered")) + 
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }     
    else if(("Confirmed" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = confirmed.tot, colour = "Confirmed")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      } 
      else if(("Deaths" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = deaths.tot, colour = "Deaths")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      } 
    else if(("Recovered" %in% show.which.cases()))
      {
        ggplot(aus.corona.all) +
          geom_line(mapping = aes(x = date, y = recovered.tot, colour = "Recovered")) +
          labs (x = "Date", y = "Cumulative cases", title = "National")
      }   
    })
  }
  # Complete app with UI and server components
  shinyApp(ui, server)
}









