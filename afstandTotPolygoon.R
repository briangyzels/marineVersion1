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
 
listWithPointsInpolygons <- data.frame(point = rownames(listWithPointsInpolygons), polygon = listWithPointsInpolygons,row.names = NULL)
 
#lijst maken van afstandmetingen
#note: Dit kan nog geoptimalizeerd worden 
lijstAfstandTotBorder <-list()
lijstAfstandTotMiddelpunt <- list()
for (j in  1:nrow(listWithPointsInpolygons)){
  coord <-coords[listWithPointsInpolygons[j,"point"]]
  polygon <-spatialpolygonsList[listWithPointsInpolygons[j,"polygon"]]
  lijstAfstandTotBorder[j]  <-  gDistance(coord,polygon)
  lijstAfstandTotMiddelpunt[j] <- gDistance(coord,gCentroid(polygon))
  
}
# gemiddelde afstand tot polygoonrand
avgDistanceToPolygonBorder <- round(mean(as.numeric(lijstAfstandTotBorder)),3)
# gemiddelde afstand tot polygooncentrum
avgDistanceToPolygonCenter <- round(mean(as.numeric(lijstAfstandTotMiddelpunt)),3)
} 

#code voor testing in loop
lijst2[j] <- listWithPointsInpolygons[j,"point"]
#hard coded testing

datadummy <- c(data[3542,"decimalLatitude"],data[3542,"decimalLongitude"])
w <- gCentroid(spatialpolygonsList[22],byid = TRUE)
w
map <-leaflet() %>%
  addTiles() %>%
  
  addMarkers(data=datadummy,lat = c(57.3117,62.84666),lng = c(12.1228,16.26404)) %>%
  addPolygons(data = shp,fillColor = "#FF0000")

map
#Probleem: zorg dat je de index van elke waarde in listWithPointsInpolygons kunt bepalen 
# zodat je coords kan gebruiken om het dichtste coordinaat tussen polygoon en waarneming
#coordinaat kunt bepalen




