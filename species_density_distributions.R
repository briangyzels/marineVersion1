#Paketten
library(gridExtra)
library(robis)
library(mregions)
library(ggmap)
register_google(key = "AIzaSyBZNTP4H1d_W0FcjrBf9yO3waT9rJ2kw60")
has_google_key()
library(ggplot2)
library(MASS)
install.packages('plotly')
library(plotly)
library(sp)
install.packages('broom')
library(broom)
library(plyr)
library(maptools)
library(rgdal)
library(worrms)
library(stringr)
library(sqldf)

install.packages("mvShapiroTest")
library(mvShapiroTest)
z <- data.frame(x, y)
z <- data.matrix(z)
p <- kde(z)
plot(p, display="filled.contour2", cont=c(25,50,75), xlim=c(-20,20), ylim=c(45,65))

plot(p)
mvShapiro.Test(head(z,5000))
mvShapiro.Test(z[5000:9999])
mvShapiro.Test(z[9999:14998])
mvShapiro.Test(z[14998:19997])

qqnorm(data$decimalLatitude)
qqline(data$decimalLatitude)
log.dla <-log(data$decimalLatitude)
qqnorm(log.dla)
qqline(log.dla)
qqnorm(data$decimalLongitude)
qqline(data$decimalLongitude)

#Witte dunschaal
data <- occurrence("Abra alba") 
northseamap <- get_map(location = "world",  maptype = "satellite", source = "google", zoom =1)
Gazp.points = tidy(gazetteerPolygons)
Gazp.df = join(Gazp.points, gazetteerPolygons@data, by="id")
distribution <- wm_distribution(wm_name2id("Abra alba"))
distinct_distribution <-sqldf("select distinct(locationID),locality from distribution")
for (i in 1:2){
  MGRID <- str_split(distinct_distribution[i,]$locationID,"/",simplify = TRUE)
  response <- GET(url = paste("http://www.marineregions.org/rest/getGazetteerWMSes.json/",MGRID[5],"/",sep="") )
  record <- content(response)
  i
  shapeprobeer <- mr_shp(
    key = paste(record[[1]]$namespace,record[[1]]$featureType, sep = ":"),  maxFeatures = 500, read = TRUE
  )
  if (i>1){
    newGazetteerPolygon  <-shapeprobeer[toupper(shapeprobeer@data[,record[[1]]$featureName]) == toupper(record[[1]]$value),]
    names(gazetteerPolygons) <- names(newGazetteerPolygon) 
    
    gazetteerPolygons <- rbind(gazetteerPolygons, newGazetteerPolygon, makeUniqueIDs = TRUE) 
  }else
  {
    gazetteerPolygons <-shapeprobeer[toupper(shapeprobeer@data[,record[[1]]$featureName]) == toupper(record[[1]]$value),]
    
  }
  
}
NSM <- ggmap(northseamap, extent = "normal", maprange = FALSE) +
  geom_polygon(data = Gazp.df,
               aes(long, lat, group = group), 
               fill = "orange", colour = "red", alpha = 0.5)
plot1 <- NSM + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.3),
  size = 0.5, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM + stat_density2d(
  aes(x = ABAL_aqm$Long, y = ABAL_aqm$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = ABAL_aqm, geom = "polygon") + geom_density2d(data = ABAL_aqm, aes(x=ABAL_aqm$Long, y = ABAL_aqm$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)

NSM <- ggmap(northseamap, extent = "normal", maprange = FALSE) +
  geom_polygon(data = shape,
               aes(long, lat, group = group), 
               fill = "orange", colour = "red", alpha = 0.5)
plot1 <- NSM + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.3),
  size = 0.5, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM + stat_density2d(
  aes(x = ABAL_aqm$Long, y = ABAL_aqm$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = ABAL_aqm, geom = "polygon") + geom_density2d(data = ABAL_aqm, aes(x=ABAL_aqm$Long, y = ABAL_aqm$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)




#Schol
data <- occurrence("Pleuronectes platessa") 
northseamap <- get_map(location = "north sea",  maptype = "satellite", source = "google", zoom =4)
NSM <- ggmap(northseamap)
plot1 <- NSM + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.5),
  size = 0.5,h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1, colour="white", alpha=0.4)   
plot2<- NSM + stat_density2d(
  aes(x = PLPL$Long, y = PLPL$Lat, fill = ..level.., alpha = 0.5),
  size = 0.2, h=1, data = PLPL, geom = "polygon") +  scale_fill_gradient(low = "red", high = "yellow") + geom_density2d(data = PLPL, aes(x=PLPL$Long, y = PLPL$Lat), size = 0.1, colour="white", alpha=0.4) 
grid.arrange(plot1, plot2, ncol=2)

NSM + stat_bkde2d(bandwidth=c(0.5, 4), data= data, aes(fill = ..level..), geom = "polygon")
# Hondshaai
data <- occurrence("Scyliorhinus canicula") 
northseamap2 <- get_map(location = "north sea",  maptype = "satellite", source = "google", zoom =3)
NSM2 <- ggmap(northseamap2)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.5, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = SCCA$Long, y = SCCA$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = SCCA, geom = "polygon") + geom_density2d(data = SCCA, aes(x=SCCA$Long, y = SCCA$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Gewone zeehond
data <- occurrence("Phoca vitulina") 
northseamap3 <- get_map(location = "north atlantic ocean",  maptype = "satellite", source = "google", zoom =3)
NSM2 <- ggmap(northseamap3)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = PHVI$Long, y = PHVI$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = PHVI, geom = "polygon") + geom_density2d(data = PHVI, aes(x=PHVI$Long, y = PHVI$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Jan-van-gent
data <- occurrence("Morus bassanus") 
northseamap3 <- get_map(location = "north atlantic ocean",  maptype = "satellite", source = "google", zoom =3)
NSM2 <- ggmap(northseamap3)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = MOBA$Long, y = MOBA$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = MOBA, geom = "polygon") + geom_density2d(data = MOBA, aes(x=MOBA$Long, y = MOBA$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Oorkwal
data <- occurrence("Aurelia aurita") 
northseamap4 <- get_map(location = "world",  maptype = "satellite", source = "google", zoom =1)
NSM2 <- ggmap(northseamap4)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = AUAU$Long, y = AUAU$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = AUAU, geom = "polygon") + geom_density2d(data = AUAU, aes(x=AUAU$Long, y = AUAU$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  

#Lederschildpad
data <- occurrence("Dermochelys coriacea") 
northseamap4 <- get_map(location = "world",  maptype = "satellite", source = "google", zoom =1)
NSM2 <- ggmap(northseamap4)
plot1 <- NSM2 + stat_density2d(
  aes(x = data$decimalLongitude, y = data$decimalLatitude, fill = ..level.., alpha = 0.1),
  size = 0.2, h=5, data = data, geom = "polygon") + geom_density2d(data = data, aes(x=data$decimalLongitude, y = data$decimalLatitude), size = 0.1)   
plot2<- NSM2 + stat_density2d(
  aes(x = DECO$Long, y = DECO$Lat, fill = ..level.., alpha = 0.1),
  size = 0.2, h=1, data = DECO, geom = "polygon") + geom_density2d(data = DECO, aes(x=DECO$Long, y = DECO$Lat), size = 0.1) 
grid.arrange(plot1, plot2, ncol=2)  



## punten
geom_point(aes(x = data$decimalLongitude, y = data$decimalLatitude), data = data, colour= "black", alpha=0.02, size = 0.2)

#bin size
install.packages('UsingR')
library(UsingR)
install.packages("tkrplot")
library(tkrplot)
source("http://homepage.divms.uiowa.edu/~luke/classes/STAT7400/examples/tkdens.R")
tkdens(x=data$decimalLongitude, y = data$decimalLatitude, tkrplot=TRUE)
install.packages("ggalt")
library(ggalt)
stat_bkde2d(mapping = NULL, data = NULL, geom = "density2d",
            position = "identity", contour = TRUE, bandwidth = NULL,
            grid_size = c(51, 51), range.x = NULL, truncate = TRUE, na.rm = FALSE,
            show.legend = NA, inherit.aes = TRUE, ...)