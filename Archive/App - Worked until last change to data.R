########################   Basic stuff for new file ##################################
##### clear env ####
rm(list = ls())

library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(lubridate)
library(openxlsx)
library(date)

##################### Fetch updated data ##########################
#### Get data from sources:
#Old depreciated sources:
# Confirmed cases: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
# Deaths: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv
# Recovered: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv
#To use now:
confirmed.raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
deaths.raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
recovered.raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), fileEncoding  = "UTF-8-BOM")

#### Prep data
# confirmed cases
confirmed <- confirmed.raw %>%
  filter(Country.Region=="Australia") %>%
  filter(Province.State!="From Diamond Princess") %>%
  select (-c(Country.Region, Lat, Long))%>%
  rename(state = Province.State)
confirmed <- gather(confirmed, key = "date", value = "confirmed", -"state")
confirmed$state <-as.character(confirmed$state)
# Deaths
deaths <- deaths.raw %>%
  filter(Country.Region=="Australia") %>%
  filter(Province.State!="From Diamond Princess") %>%
  select (-c(Country.Region, Lat, Long))%>%
  rename(state = Province.State)
deaths <- gather(deaths, key = "date", value = "deaths", -"state")
deaths$state <-as.character(deaths$state)
# Recovered
names(recovered.raw)[1] <- "Province.State"
recovered <- recovered.raw %>%
  filter(Country.Region=="Australia") %>%
  filter(Province.State!="From Diamond Princess") %>%
  select (-c(Country.Region, Lat, Long))%>%
  rename(state = Province.State)
recovered <- gather(recovered, key = "date", value = "recovered", -"state")
recovered$state <-as.character(recovered$state)
# fix date inconsistency
recovered$date <-  gsub("2020", "20", recovered$date)
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
# Calculate active cases
aus.corona$active <- aus.corona$confirmed - aus.corona$deaths - aus.corona$recovered
# Make total and daily
aus.corona$type <-"cumulative"
# Calculate new cases
aus.corona.new <- aus.corona%>%
  group_by(state)%>%
  mutate(confirmed.lag1 = lag(confirmed, n = 1, order_by = date)) %>%
  mutate(deaths.lag1 = lag(deaths, n = 1, order_by = date)) %>%
  mutate(recovered.lag1 = lag(recovered, n = 1, order_by = date)) %>%
  mutate(active.lag1 = lag(active, n = 1, order_by = date))
# Overwrite cumaltive with new daily case
aus.corona.new$confirmed <- aus.corona.new$confirmed - aus.corona.new$confirmed.lag1
aus.corona.new$deaths <- aus.corona.new$deaths - aus.corona.new$deaths.lag1
aus.corona.new$recovered <- aus.corona.new$recovered - aus.corona.new$recovered.lag1
aus.corona.new$active <- aus.corona.new$active - aus.corona.new$active.lag1
aus.corona.new$type <- "daily"
aus.corona.new <- aus.corona.new[,c("state", "date", "confirmed", "deaths", "recovered","active", "type")]
# combine the two cumulative and new
aus.corona <- dplyr::bind_rows(aus.corona, aus.corona.new)
# make it long to be able to use levels in ggplot
aus.corona <- aus.corona%>%
  gather(key = status, value = no.of.cases, -c(state, date, type))
# Make capitalized status for prettier labels
aus.corona$status[aus.corona$status=="confirmed"] <-"Confirmed"
aus.corona$status[aus.corona$status=="active"] <-"Active"
aus.corona$status[aus.corona$status=="deaths"] <-"Deaths"
aus.corona$status[aus.corona$status=="recovered"] <-"Recovered"
# Ordered levels
aus.corona$status <- factor(aus.corona$status, levels = c("Confirmed", "Active", "Recovered", "Deaths"))
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




#############################  Begin Much better graph style   ###################################
theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.key.size= unit(0.4, "cm"),
            legend.text = element_text(size=14),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
#############################  END Much better graph style   ###################################
# Common themes
mylinesize = 1.25
mydotsize = 2.5
mytweaks <- theme(legend.title = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15))+
  theme(axis.title.x = element_blank()) 
mylabs <-  scale_x_date(date_labels = "%d/%m")
myaxes <- scale_y_continuous(sec.axis = sec_axis(~.*1))

#mylabs <-  scale_x_date(date_labels = "%x")

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
                         choices = c("Confirmed","Active", "Deaths", "Recovered"),
                         selected = c("Confirmed","Active", "Deaths", "Recovered")),
      # Select cumulative or daily
      radioButtons("showserieschoice", "Type of cases:",
                   choices = c("Cumulative cases" = "cumulative",
                               "Daily increase" = "daily"),
                   selected = "cumulative"),
      # Add a Slider Input to select date range
      sliderInput("daterange", "Date range shown:",
                  min = date.min,
                  max = date.max,
                  value = c(date.max-21, date.max)),
      h4("About this app"),
      h5("This app was written by Jorrit Gosens."),
      h5("The data used in this tracker is compiled by the"),
      h5(tags$a("Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")),
      h5("See also their publication here:"),
      h5(tags$a("An interactive web-based dashboard to track COVID-19 in real time", href="https://doi.org/10.1016/S1473-3099(20)30120-1"),
         h5("This data is updated daily."),
         h5("NB: Data on recovered cases is updated only untill 22/03, as Johns Hopkins University has made changes to the way darta is reported. This will be updated again when new data becomes available.")
      )
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
  ### Check what date range and what type of cases, dates, and series (cumulative vs daily) is selected and update dataframe to be plotted
  nat.filtered <- reactive({aus.corona.nat %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  act.filtered <- reactive({aus.corona.act %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  nsw.filtered <- reactive({aus.corona.nsw %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  nt.filtered <- reactive({aus.corona.nt %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  qld.filtered <- reactive({aus.corona.qld %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  sa.filtered <- reactive({aus.corona.sa %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  tas.filtered <- reactive({aus.corona.tas %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  vic.filtered <- reactive({aus.corona.vic %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})
  wa.filtered <- reactive({aus.corona.wa %>%filter(between(date, input$daterange[1], input$daterange[2]))%>%filter(type == input$showserieschoice)%>% filter(status %in% input$showcasechoice)})

  ##################################################   All those plots ###############################################
  ##################################################     National   ###################################################
  output$plot.nat <- renderPlot({
    ggplot(data = nat.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "National")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()
  })
  ##################################################     NSW   ###################################################
  output$plot.nsw <- renderPlot({
    ggplot(data = nsw.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "New South Wales")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     Victoria   ###################################################
  output$plot.vic <- renderPlot({
    ggplot(data = vic.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "Victoria")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     Queensland   ###################################################
  output$plot.qld <- renderPlot({
    ggplot(data = qld.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "Queensland")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     Western Australia   ###################################################
  output$plot.wa <- renderPlot({
    ggplot(data = wa.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "Western Australia")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     South Australia   ###################################################
  output$plot.sa <- renderPlot({
    ggplot(data = sa.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "South Australia")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     Tasmania   ###################################################
  output$plot.tas <- renderPlot({
    ggplot(data = tas.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "Tasmania")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     Australian Capital Territory   ###################################################
  output$plot.act <- renderPlot({
    ggplot(data = act.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "Australian Capital Territory")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })
  ##################################################     Northern Territory  ###################################################
  output$plot.nt <- renderPlot({
    ggplot(data = nt.filtered())+
      aes(x = date)+ geom_line(aes(y = no.of.cases, group=status, color=status),size=mylinesize)+geom_point(aes(y = no.of.cases, group=status, color=status),size=mydotsize)+
      labs (x = "Date", y = "Number of cases", title = "Northern Territory")+mytweaks+mylabs+myaxes+
      scale_colour_Publication()+ theme_Publication()

  })

  # Bracket to end whole server section
}
# Run the application
shinyApp(ui = ui, server = server)