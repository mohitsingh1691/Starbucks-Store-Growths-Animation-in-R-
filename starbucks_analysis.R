

library(ggplot2)
library(tidyverse)
library(lubridate)
library(animation)
library(stringr)
library(ggrepel)
library(maptools)
library(ggmap)


starbucks <- read.csv("starbucks.csv")

starbucks_dat <- starbucks %>%
  filter(Country.Subdivision != "AK",Country.Subdivision != "HI" )

starbucks_dat <-na.omit(starbucks_dat)
starbucks_dat$First.Seen <- format(as.POSIXct(starbucks_dat$First.Seen,format='%m/%d/%Y'),format='%m/%d/%Y')
starbucks_dat$First.Seen <-as.Date(starbucks_dat$First.Seen,format='%m/%d/%Y')

starbucks_dat <- starbucks_dat[starbucks_dat$First.Seen>ymd("2015-01-01"),]


# Get the coordinates of major cities of US (more than 300k inhabitants)
major_cities <- maps::us.cities %>%
  filter(country.etc != "HI", country.etc != "AK", pop > 300000) %>%
  mutate(name = str_replace(name, " [A-Z][A-Z]", ""))

major_cities <-na.omit(major_cities)

starbucks_dat$City <-as.character(starbucks_dat$City)
starbucks_dat$Country.Subdivision <-as.character(starbucks_dat$Country.Subdivision)

# Get coordinates of US cities where more than 2 stores were opened from 2013-2016
starbucks_dat1 <- maps::us.cities %>%
  mutate(city = str_replace(name, " [A-Z][A-Z]", "")) %>%
  inner_join(starbucks_dat, c("city" = "City", "country.etc" = "Country.Subdivision")) %>%
  group_by(city, country.etc) %>%
  summarise(count = n(), long = Longitude[1], lat = Latitude[1]) %>%
  ungroup() %>%
  filter(count > 2)

mm <-data.frame(map_data("usa"))

# Create base layer of US map
plot_star_loc <- ggplot() + 
  geom_polygon(data = map_data("usa"), aes(long, lat, group = group), fill = "#00704a") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  coord_quickmap()

plot_star_loc

# Plot the store locations and show major cities
plot_star_loc +
  geom_text_repel(data = major_cities, aes(long, lat, label = name), size = 5) +
  geom_point(data = starbucks_dat, aes(Longitude, Latitude), alpha = 0.3, color = "#d42426") +
  ggtitle("Starbucks store growth 2013-2016")


# Plot deaths and show most deadly cities
plot_star_loc +
  geom_text_repel(data = starbucks_dat1, aes(long, lat, label = city), size = 4) +
  geom_point(data = starbucks_dat, aes(Longitude, Latitude), alpha = 0.3, color = "#d42426") +
  ggtitle("Starbucks Store Openings since 2013")

#Animation - Showing the growth of store in 2016
saveGIF(for (i in 0:300) {

starbucks_time <- starbucks_dat %>%
    filter(First.Seen >= ymd("2016-01-01") + i)
  
  starbucks_cities <- starbucks_dat1 %>%
    left_join(starbucks_time, c("city" = "City", "country.etc" = "Country.Subdivision")) %>%
    group_by(city, country.etc) %>%
    summarise(count = n(), long = Longitude[1], lat = Latitude[1]) %>%
    ungroup() %>%
    mutate(alph = count > 3)
  

print(plot_star_loc +
          geom_text_repel(data = starbucks_cities, size = 5, segment.alpha = 0, 
                          aes(long, lat, label = city, alpha = factor(alph))) +
          scale_alpha_manual(values = c(0, 1)) +
          geom_point(data = starbucks_time, aes(Longitude, Latitude), alpha = 0.4, color = "#d42426") +
          ggtitle(paste0("Starbucks Stores as first seen on ", ymd("2016-01-01") + i)))
  
}, "starbucks4.gif", interval = 0.006, ani.width = 1000, ani.height = 730)

