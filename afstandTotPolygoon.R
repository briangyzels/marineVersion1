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
install.packages("sp")
install.packages("spdep")
install.packages("rgeos")
install.packages("cartography")
library("cartography")
library("rgeos")
library("plyr")
library("httr")
library("jsonlite")
library("robis")
library("sf")
library("mapview")
library("mregions")
library("gdalUtils")
library("sp")
library("spdep")

#Robis package
library("robis", lib.loc="~/R/win-library/3.5")
library("leaflet")

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
data <- occurrence("Abra alba")  
keys <- c("MarineRegions:longhurst")
,"MarineRegions:eez"
fit = list()
values = list()
for (i in 1:length(keys)){
  shp <- mr_shp(key = keys[i])
  spatialpolygonsList <- as.SpatialPolygons.PolygonsList(shp@polygons, proj4string=CRS(as.character(NA)))  
  pointsWithPolygonsJoin <- sp::over(coords,spatialpolygonsList)
  fit[i] <- round(x=length(pointsWithPolygonsJoin[complete.cases(pointsWithPolygonsJoin)]) / length(coords),2)
listWithPointsInpolygons <-  pointsWithPolygonsJoin[complete.cases(pointsWithPolygonsJoin)]
spatialpolygonsList@polygons[1]

getBorders()
# for (j in 1:length(listWithPointsInpolygons)){
#    st_nearest_points(coords[1], listWithPointsInpolygons[i])
#   
# }  
} 

#Probleem: zorg dat je de index van elke waarde in listWithPointsInpolygons kunt bepalen 
# zodat je coords kan gebruiken om het dichtste coordinaat tussen polygoon en waarneming
#coordinaat kunt bepalen

 listWithPointsInpolygons 
pointsWithPolygonsJoin
values[i] <-  apply(gDistance(coords, spatialpolygonsList,byid=TRUE),2,min)
r = sqrt(2)/10
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))

b1 <- st_buffer(pt1, r)
b2 <- st_buffer(pt2, r)
st_nearest_points(b2, b1)


