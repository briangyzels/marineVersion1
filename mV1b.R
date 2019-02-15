install_github("iobis/robis")
install.packages("mregions")
install.packages("httr")
install.packages("jsonlite")
install.packages("sf")
install.packages("mapview")
install.packages("gdalUtils") 
install.packages("rmapshaper")
install.packages("leaflet")
install.packages("plyr")
library("plyr")
library("httr")
library("jsonlite")
library("robis")
library("sf")
library("mapview")
library("mregions")
library("gdalUtils")
library("robis", lib.loc="~/R/win-library/3.5")
library("leaflet")

#change1
#Robis package

 
 data <- occurrence("Abra alba") 
#check of waarnemingen in een polygoon liggen
#werkt
d <- data.frame(data$decimalLatitude,data$decimalLongitude)
d
#werkt niet
# types <- mr_place_types()
# head(places$type)
# 
# x <- grep("LONGHURST", types$type,ignore.case = TRUE, value = TRUE)
# res <- mr_records_by_type(type=x)
# res  
# map waarnemingen en polygonen op 1 kaart
#zonder loop
keys <- c("MarineRegions:longhurst","MarineRegions:eez")
fit = list()
 
for (i in 1:length(keys)){
 shp <- mr_shp(key = keys[i])
s <- as.SpatialPolygons.PolygonsList(shp@polygons, proj4string=CRS(as.character(NA)))  
a <- sp::over(coords,s)
fit[i] <- round(x=length(a[complete.cases(a)]) / length(coords),2)

} 
#zonder loop
shp <- mr_shp(key = "MarineRegions:longhurst")

# leaflet() %>%
#   addTiles() %>%
#   addMarkers(data=data,lng = ~decimalLongitude,lat = ~decimalLatitude) %>%
#   addPolygons(data = shp)

coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
 
s <- as.SpatialPolygons.PolygonsList(shp@polygons, proj4string=CRS(as.character(NA))) 
#check of punten in polygonen zitten
a <- sp::over(coords,s) 
# fitting
fit <- round(x=length(a[complete.cases(a)]) / length(coords),2) 
fit
  