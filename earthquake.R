#Topic: Mapping Significant Earthquakes from 1965 to 2016
# Earthquake is concentrated in a few earthquake zones


#load llibraries
library(plyr)
library(ggalt)
library(ggplot2)
library(maps)
library(readr)
library(dplyr)
library(ggmap)
library(sp)
library(rworldmap)
df.erathquake = read_csv("/Users/Lucinda/Desktop/FE550/data_viz_R/database.csv")

#GET WORLD MAP
map_world = map_data("world")


#----------Data Manipulation--------------



coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  
  
  # convert our list of points to a SpatialPoints object
  
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

frequency_region =count(coords2country(data.frame( df.erathquake$Longitude, df.erathquake$Latitude)))
new_data = frequency_region[1:20, ]
#----------HISTOGRAM----------------------



ggplot(data = new_data, aes(x = reorder(x, -freq), y = freq),fill = df.erathquake$`Magnitude Type`) +
  geom_bar(stat="identity",
           col = "red",
           fill = "green",
           alpha = 0.2) +
  labs(title="Histogram for Sum of Earthquake in Top 30") +
  labs( y="Total Number")

#----------WORLD MAP----------------------



# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees


#SIMPLE PLOT WITH POINTS
ggplot()+
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.erathquake, aes(x = Longitude, y = Latitude), color = "red")

#-----------------------------------------
# CREATE FINAL MAP
# notes:
# 1. size is the total VC investment
# 2. there are two layers of points.  This is to have both the
#    point outline as well as an interior, but with different 
#    transparency levels (i.e., different alpha)
# 3. most of the formatting (i.e., theming) is just removing things
#    to make this simpler
#-----------------------------------------

ggplot()+
  ggtitle("Map of Significant Earthquakes 1965-2016")+
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group), fill ="#002035",colour = "#114151", size = 0.25 ) +
  geom_point(data = df.erathquake, aes(x = Longitude, y = Latitude, size = Magnitude), color = "dark red", alpha = 0.15)+
  geom_point(data = df.erathquake, aes(x = Longitude, y = Latitude, size = Magnitude), color = "dark red", alpha = 0.8, shape = 1)+
  scale_size_continuous(range = c(1,7), breaks = c(6,7,8,9)) +
  theme(text = element_text(family = "Gill Sans")) +
  theme(panel.background = element_rect(fill = "grey90"))+
  theme(panel.grid = element_blank())+
  theme(axis.text = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(axis.title = element_blank())+
  theme(legend.background = element_blank())+
  theme(legend.key = element_blank()) +
  theme(legend.title = element_text(size = 14, color="#666666", face="bold")) +
  theme(legend.text = element_text(size = 12, color="#666666", face="bold")) +
  theme(plot.title = element_text(family = "Gill Sans", color="#666666", face="bold", size=26, hjust=0)) 
  

