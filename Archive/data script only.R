##### clear env ####
rm(list = ls())

# Load packages
library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

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


