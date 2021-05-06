##### clear env ####
rm(list = ls())

# Load packages
library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

# Save and use for now
aus.corona <- read_csv("C:/Users/jorri/OneDrive/Work/shiny corona aus/auscoronatemp.csv")


########################################
# List of States to loop over
statelist <- c("National", "Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")
plotno = 1
plot <- ""

for (i in statelist) {
  temp.df <- aus.corona%>%filter(aus.corona$state==i)
  tempplot <- ggplot(temp.df) +
    geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) +
    geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) + 
    geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) + 
    labs (x = "Date", y = "Cumulative cases", title = i)
  plot <- plot + tempplot
  plotno = plotno + 1
}
print(plot) 