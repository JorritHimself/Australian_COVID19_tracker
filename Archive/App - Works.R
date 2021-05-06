# Load packages
library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)

##################### Fetch updated data ##########################
#### Get data from sources:
# Confirmed cases: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
# Deaths: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv
# Recovered: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv
confirmed.raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
deaths.raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
recovered.raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))


#### Prep data
# confirmed cases
confirmed <- confirmed.raw %>%
  filter(Country.Region=="Australia") %>%
  filter(Province.State!="From Diamond Princess") %>%
  select (-c(Country.Region, Lat, Long))%>%
  rename(state = Province.State)
confirmed <- gather(confirmed, key = "date", value = "confirmed", -"state")
# Deaths
deaths <- deaths.raw %>%
  filter(Country.Region=="Australia") %>%
  filter(Province.State!="From Diamond Princess") %>%
  select (-c(Country.Region, Lat, Long))%>%
  rename(state = Province.State)
deaths <- gather(deaths, key = "date", value = "deaths", -"state")
# Deaths
recovered <- recovered.raw %>%
  filter(Country.Region=="Australia") %>%
  filter(Province.State!="From Diamond Princess") %>%
  select (-c(Country.Region, Lat, Long))%>%
  rename(state = Province.State)
recovered <- gather(recovered, key = "date", value = "recovered", -"state")
# Put it together
aus.corona <- left_join(confirmed, deaths, by = c("state", "date"))
aus.corona <- left_join(aus.corona, recovered, by = c("state", "date"))
# Fix State factor levels mess
aus.corona$state <- as.character(aus.corona$state)
aus.corona$state <- as.factor(aus.corona$state)
# fix datemess
aus.corona$date <- gsub("X", "0", aus.corona$date)
aus.corona$date <- as.Date(aus.corona$date, "%m.%d.%y")
# Calculate national totals
temp.sum.nat <- aus.corona%>%
  group_by(date)%>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered))
temp.sum.nat$state <- "National"
aus.corona <- rbind(aus.corona, temp.sum.nat)
# Make total and daily
aus.corona$type <-"cumulative"
# Calculate new cases
aus.corona.new <- aus.corona%>%
  group_by(state)%>%
  mutate(confirmed.lag1 = lag(confirmed, n = 1, order_by = date)) %>%
  mutate(deaths.lag1 = lag(deaths, n = 1, order_by = date)) %>%
  mutate(recovered.lag1 = lag(recovered, n = 1, order_by = date))
# Overwrite cumaltive with new daily case
aus.corona.new$confirmed <- aus.corona.new$confirmed - aus.corona.new$confirmed.lag1
aus.corona.new$deaths <- aus.corona.new$deaths - aus.corona.new$deaths.lag1
aus.corona.new$recovered <- aus.corona.new$recovered - aus.corona.new$recovered.lag1
aus.corona.new$type <- "daily"
aus.corona.new <- aus.corona.new[,c("state", "date", "confirmed", "deaths", "recovered","type")]
# combine the two cumulative and new
aus.corona <- dplyr::bind_rows(aus.corona, aus.corona.new)
# separate files for each state
aus.corona.nat <- aus.corona%>%filter(state == "National")
aus.corona.act <- aus.corona%>%filter(state == "Australian Capital Territory")
aus.corona.nsw <- aus.corona%>%filter(state == "New South Wales")
aus.corona.nt <- aus.corona%>%filter(state == "Northern Territory")
aus.corona.qld <- aus.corona%>%filter(state == "Queensland")
aus.corona.sa <- aus.corona%>%filter(state == "South Australia")
aus.corona.tas <- aus.corona%>%filter(state == "Tasmania")
aus.corona.vic <- aus.corona%>%filter(state == "Victoria")
aus.corona.wa <- aus.corona%>%filter(state == "Western Australia")
# Get min and max dates in the full sample
date.max <- max(aus.corona$date, na.rm = TRUE)
date.min <- min(aus.corona$date, na.rm = TRUE)

# Common themes
mylinesize = 1.25
mydotsize = 2.5
mytweaks <- theme(legend.title = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15))+
  theme(axis.title.x = element_blank())

###################################        The shiny bits ###################################
# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Australian COVID-19 case tracker"),
  # Sidebar with slider input to select date range
  sidebarLayout(
    sidebarPanel(
      # Select types of cases
      checkboxGroupInput("showcasechoice", "Status:", 
                         choices = c("Confirmed", "Deaths", "Recovered"),
                         selected = c("Confirmed", "Deaths", "Recovered")),
      # Select cumulative or daily
      radioButtons("showserieschoice", "Type of cases:", 
                   choices = c("Cumulative cases" = "cumulative",
                               "Daily cases" = "daily"),
                   selected = "cumulative"),
      # Add a Slider Input to select date range
      sliderInput("daterange", "Date range shown:",
                  min = date.min,
                  max = date.max,
                  value = c(date.min, date.max))
    ),
    
    # Show a plot of the trend
    mainPanel(
      plotOutput("plot.nat"),
      plotOutput("plot.nsw"),
      plotOutput("plot.vic"),
      plotOutput("plot.qld"),
      plotOutput("plot.wa"),
      plotOutput("plot.sa"),
      plotOutput("plot.tas"),
      plotOutput("plot.act"),
      plotOutput("plot.nt")
      )
  )
)

server <- function(input, output, session) {
  ###Check what boxes are ticked for data types
  show.which.cases <- reactive({
    as.vector(input$showcasechoice)
  }) 
  show.type.cases <- reactive({
    as.vector(input$showserieschoice)
  }) 
  ###Check what date range and what type of data is selected and update dataframe to be plotted
  nat.filtered <- reactive({aus.corona.nat %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  act.filtered <- reactive({aus.corona.act %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  nsw.filtered <- reactive({aus.corona.nsw %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  nt.filtered <- reactive({aus.corona.nt %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  qld.filtered <- reactive({aus.corona.qld %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  sa.filtered <- reactive({aus.corona.sa %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  tas.filtered <- reactive({aus.corona.tas %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  vic.filtered <- reactive({aus.corona.vic %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})
  wa.filtered <- reactive({aus.corona.wa %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)})

  ############### All those plots
  ##################################################     National   ###################################################
  output$plot.nat <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = nat.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "National")+mytweaks
    } 
  })
  ##################################################     ACT   ###################################################
  output$plot.act <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = act.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks
    } 
  })
  ##################################################     New South Wales   ###################################################
  output$plot.nsw <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = nsw.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks
    } 
  })
  ##################################################     Northern Territory   ###################################################
  output$plot.nt <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = nt.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks
    } 
  })
  ##################################################     Queensland   ###################################################
  output$plot.qld <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = qld.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks
    } 
  })
  ##################################################     South Australia   ###################################################
  output$plot.sa <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = sa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks
    } 
  })
  ##################################################     Tasmania   ###################################################
  output$plot.tas <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = tas.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks
    } 
  })
  ##################################################     Victoria   ###################################################
  output$plot.vic <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = vic.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks
    } 
  })
  ##################################################     Western Australia   ###################################################
  output$plot.wa <- renderPlot({
    if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    }
    else if(("Confirmed" %in% show.which.cases()) & ("Recovered" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases()) & ("Deaths" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    } 
    
    else if(("Confirmed" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = confirmed, colour = "Confirmed"),size=mylinesize)+geom_point(aes(y = confirmed, colour = "Confirmed"),size=mydotsize)+
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    }
    else if(("Deaths" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = deaths, colour = "Deaths"),size=mylinesize)+ geom_point(aes(y = deaths, colour = "Deaths"))+
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    } 
    else if(("Recovered" %in% show.which.cases())){
      ggplot(data = wa.filtered())+ 
        aes(x = date)+
        geom_line(aes(y = recovered, colour = "Recovered"),size=mylinesize)+geom_point(aes(y = recovered, colour = "Recovered")) +
        labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks
    } 
  })

### Bracket to end whole server section  
}
# Run the application 
shinyApp(ui = ui, server = server)