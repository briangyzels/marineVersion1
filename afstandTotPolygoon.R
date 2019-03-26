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
install.packages("rindex")
install.packages("data.table", dependencies=TRUE)
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
data <- occurrence("Solea solea") 
d <- data.frame(data$decimalLatitude,data$decimalLongitude)
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
#werkt niet
# types <- mr_place_types()
# head(places$type)
# 
# x <- grep("LONGHURST", types$type,ignore.case = TRUE, value = TRUE)
# res <- mr_records_by_type(type=x)
# res  
# map waarnemingen en polygonen op 1 kaart
shpdummy <- mr_shp(key = "MarineRegions:lme" )
 
keys <- c("MarineRegions:lme")
# ,"MarineRegions:longhurst"
fit = list()
values = list()
#fit bereken voor elke polygoonsoort
for (i in 1:length(keys)){
  shp <- mr_shp(key = keys[i])
  spatialpolygonsList <- as.SpatialPolygons.PolygonsList(shp@polygons, proj4string=CRS(as.character(NA)))  
  pointsWithPolygonsJoin <- sp::over(coords,spatialpolygonsList)
  fit[i] <- round(x=length(pointsWithPolygonsJoin[complete.cases(pointsWithPolygonsJoin)]) / length(coords),2)
listWithPointsInpolygons <- data.frame(polygon = pointsWithPolygonsJoin[complete.cases(pointsWithPolygonsJoin)]) 
 
listWithPointsInpolygons <- data.frame(point = rownames(listWithPointsInpolygons), polygon = listWithPointsInpolygons)
  

#lijst maken van afstandmetingen
#note: Dit kan nog geoptimalizeerd worden 
lijst2 <-list()
lijstAfstandTotMiddelpunt <- list()
for (j in  1:nrow(listWithPointsInpolygons)){
  
  lijst[j]  <-  gDistance(coords[listWithPointsInpolygons[j,"point"]],spatialpolygonsList[listWithPointsInpolygons[j,"polygon"]])
  lijstAfstandTotMiddelpunt[j] <-gDistance(coords[listWithPointsInpolygons[j,"point"]],gCentroid(spatialpolygonsList[listWithPointsInpolygons[j,"polygon"]]))
  lijst2[j] <- listWithPointsInpolygons[j,"point"]
}
# gemiddelde afstand tot polygoonrand
avgDistanceToPolygonBorder <- round(mean(as.numeric(lijst)),3)
# gemiddelde afstand tot polygooncentrum
avgDistanceToPolygonCenter <- round(mean(as.numeric(lijstAfstandTotMiddelpunt)),3)
} 

#hard coded testing
datadummy <- c(data[3542,"decimalLatitude"],data[3542,"decimalLongitude"])

map <-leaflet() %>%
  addTiles() %>%
  
  addMarkers(data=datadummy,lat = 57.3117,lng = 12.1228) %>%
  addPolygons(data = shp,fillColor = "#FF0000")

map
#Probleem: zorg dat je de index van elke waarde in listWithPointsInpolygons kunt bepalen 
# zodat je coords kan gebruiken om het dichtste coordinaat tussen polygoon en waarneming
#coordinaat kunt bepalen




