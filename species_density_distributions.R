#Paketten
library(gridExtra)
library(robis)
library(mregions)
library(ggmap)
register_google(key = "...")
has_google_key()
library(ggplot2)

#Witte dunschaal
data <- occurrence("Abra alba") 
northseamap <- get_map(location = "north sea",  maptype = "satellite", source = "google", zoom =4)
NSM <- ggmap(northseamap)
plot1 <- NSM + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.3),
  size = 0.5, bins = 800, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM + stat_density2d(
  aes(x = ABAL_aqm$Long, y = ABAL_aqm$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 8, data = ABAL_aqm, geom = "polygon") + geom_density2d(data = ABAL_aqm, aes(x=ABAL_aqm$Long, y = ABAL_aqm$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)

#Schol
data <- occurrence("Pleuronectes platessa") 
northseamap <- get_map(location = "north sea",  maptype = "satellite", source = "google", zoom =4)
NSM <- ggmap(northseamap)
plot1 <- NSM + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.3),
  size = 0.5, bins = 800, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM + stat_density2d(
  aes(x = PLPL$Long, y = PLPL$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 8, data = PLPL, geom = "polygon") + geom_density2d(data = PLPL, aes(x=PLPL$Long, y = PLPL$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)

# Hondshaai
data <- occurrence("Scyliorhinus canicula") 
northseamap2 <- get_map(location = "north sea",  maptype = "satellite", source = "google", zoom =3)
NSM2 <- ggmap(northseamap2)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.5, bins = 80, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = SCCA$Long, y = SCCA$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 80, data = SCCA, geom = "polygon") + geom_density2d(data = SCCA, aes(x=SCCA$Long, y = SCCA$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Gewone zeehond
data <- occurrence("Phoca vitulina") 
northseamap3 <- get_map(location = "north atlantic ocean",  maptype = "satellite", source = "google", zoom =3)
NSM2 <- ggmap(northseamap3)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = PHVI$Long, y = PHVI$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = PHVI, geom = "polygon") + geom_density2d(data = PHVI, aes(x=PHVI$Long, y = PHVI$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Jan-van-gent
data <- occurrence("Morus bassanus") 
northseamap3 <- get_map(location = "north atlantic ocean",  maptype = "satellite", source = "google", zoom =3)
NSM2 <- ggmap(northseamap3)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = MOBA$Long, y = MOBA$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = MOBA, geom = "polygon") + geom_density2d(data = MOBA, aes(x=MOBA$Long, y = MOBA$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Oorkwal
data <- occurrence("Aurelia aurita") 
northseamap4 <- get_map(location = "world",  maptype = "satellite", source = "google", zoom =1)
NSM2 <- ggmap(northseamap4)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = AUAU$Long, y = AUAU$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = AUAU, geom = "polygon") + geom_density2d(data = AUAU, aes(x=AUAU$Long, y = AUAU$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Lederschildpad
data <- occurrence("Dermochelys coriacea") 
northseamap4 <- get_map(location = "world",  maptype = "satellite", source = "google", zoom =1)
NSM2 <- ggmap(northseamap4)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = DECO$Long, y = DECO$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, bins = 100, data = DECO, geom = "polygon") + geom_density2d(data = DECO, aes(x=DECO$Long, y = DECO$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  



## punten
geom_point(aes(x = data$decimalLongitude, y = data$decimalLatitude), data = data, colour= "black", alpha=0.02, size = 0.2)
