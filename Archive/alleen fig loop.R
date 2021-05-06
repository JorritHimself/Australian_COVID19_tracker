
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

########################################
# List of States to loop over
statelist <- c("Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")






for (i in statelist) {
  temp.df <- aus.corona%>%filter(aus.corona$State==i)
  plot <- ggplot(temp.df) +
    geom_line(mapping = aes(x = date, y = confirmed, colour = "Confirmed")) +
    geom_line(mapping = aes(x = date, y = deaths, colour = "Deaths")) + 
    geom_line(mapping = aes(x = date, y = recovered, colour = "Recovered")) + 
    labs (x = "Date", y = "Cumulative cases", title = i)
  print(plot)
}

