library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(tidyr)
library(randomcoloR)
register_google(key = '') #Add Google API
locations <- read_excel('DSPG_locations.xlsx')
locations_df <- separate(locations, 'Coordinates', into = c('Lat', 'Long'), sep = ', ')
locations_df <- as_tibble(locations_df)
locations_df$Lat <- as.numeric(locations_df$Lat)
locations_df$Long <- as.numeric(locations_df$Long)
locations_df <- locations_df[order(locations_df$Lat, decreasing = TRUE),]

locations_sub <- locations_df[locations_df$Location == 'Deschutes River - Biggs' | 
                                locations_df$Location == 'Deschutes River - Culver' |
                                locations_df$Location == 'Deschutes River - Madras', ]

my_colors <- distinctColorPalette(length(locations_df$Location))
my_shapes <- rep_len(0:6, length(locations_df$Location))
locations_df$shapes <- as.factor(my_shapes)
names(my_shapes) <- locations_df$Location
map <- get_googlemap(center = c(-121, 45), zoom = 8)

#All Water Temperature Locations
#Sherars Falls is in the file but is not water temp site
ggmap(map) + theme_bw() +
  geom_point(data = subset(locations_df, Location != 'Sherars Falls') , aes(x = Long, y = Lat, color = Location, shape = Location), 
             size = 5, stroke = 1.5, alpha = 1) +
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Water Temperature Sites')

#Biggs, Culver, Madras Map
ggmap(map) + theme_bw() +
  geom_point(data = locations_sub, aes(x = Long, y = Lat, color = Location), 
             shape = 6, size = 6, stroke = 1.5) + 
  labs(title = 'Biggs, Culver, Madras', x = 'Longitude', y = 'Latitude') + 
  ylim(44.25, 45.75) + xlim(-122, -120)

#SHERARS FALLS
sherars <- subset(locations_df, Location == 'Sherars Falls' | Location == 'Madras')
map2 <- get_googlemap(center = c(-121, 45), zoom = 9)
ggmap(map2) + theme_bw() +
  geom_point(data = sherars, aes(x = Long, y = Lat, color = Location), 
             shape = 6, size = 4, stroke = 1) + 
  geom_text(data = sherars, aes(x = Long, y = Lat, label = Location), hjust=-.25, vjust=0) +
  labs(title = 'Sherars Falls and Madras', x = 'Longitude', y = 'Latitude') 
