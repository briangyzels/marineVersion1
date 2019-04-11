install.packages("igraph")
install_github("raquamaps/aquamapsdata")
install.packages("data.table") 
install.packages("devtools")
install_github("ropenscilabs/mregions")
install.packages("curl")
 install.packages("data.table")
  
 install.packages("sqldf")
library("aquamapsdata")
library("purrr")

library("igraph")
library("devtools")
library(rgdal)
library(maptools)
 
 
library('httr')
library('data.table')

library("worrms")
library(rgeos)
library("mregions")
library("robis")
library('leaflet')
 library("sqldf")

distribution <- wm_distribution(wm_name2id("Abra alba"))
 
#39 plaatsen zoeken in deze en plotten op kaart
#https://recology.info/2016/06/marine-regions/
 
mr_names_search(rnames, "eez")
#ophalen van waarneming doormiddel van robis pakket en vervolgens spatialpoint van te maken
data <- occurrence("Abra alba") 
d <- data.frame(data$decimalLongitude,data$decimalLatitude)
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
#shape file ophalen MEOW  
shapeEcoregions<- mr_shp(key= "Ecoregions:ecoregions", maxFeatures = 1000)
shp <- mr_shp(key = "3293")
#ophalen een specifiek gebied gazetteer
shapefilter <- mr_shp(
  key = "MarineRegions:eez",
  filter ="Dutch Exclusive Economic Zone",  maxFeatures = 300
)
#fit van 1 polygon met waarnemingen
spatialpolygonsList <- as.SpatialPolygons.PolygonsList(shapefilter@polygons, proj4string=CRS(as.character(NA)))  
pointsWithPolygonsJoin <- sp::over(coords,spatialpolygonsList)
fit <- round(x=length(pointsWithPolygonsJoin[complete.cases(pointsWithPolygonsJoin)]) / length(coords),2)
 

#plot met polygonen met MEOW polygonen in het groen, een polygoon van de gazetteer in het blauw en waarnemingen in het rood
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = coords, radius = 3.5, weight = 0, fillOpacity = 1, fillColor = "#cc3300") %>%
  addPolygons(data = shapefilter) %>%
  addPolygons(data = shapeEcoregions,color = "green") 

# lat = data$decimalLatitude, lng = data$decimalLongitude
#MEOW, LME, ... polygonen selecteren die overlappen met gebied uit gazetteer

wkt <- mr_as_wkt(shapeEcoregions)

 
 
sizeGazetteerRegion <- gArea(readWKT(wkt))

intersect <- gIntersection(shapefilter,shapeEcoregions)

intersectSize <- gArea(intersect)
overlapping <- round(intersectSize/sizeGazetteerRegion , 2 )
   
#http://www.imachordata.com/meow-its-marine-ecoregions-in-r-2/ 


mr_names("MarineRegions:eez")
# get Samoan Exclusive Economic Zone

 
res <-subset(res,res$geoname == "Dutch Exclusive Economic Zone") 
rr <- mr_shp(key = "MarineRegions:eez_iho_union_v2",
             filter = "North Atlantic Ocean")
sp::plot(res) 
mr_names_search("EEZ", "Dutch Exclusive Economic Zone")
#85668 mgrid



#observaties + polygoon
obsInMEOW <-  obsJoinMEOW[lapply(obsJoinMEOW,length)>0]



#data frame met polygoon id en aantal obs
dt <- t(obsInMEOW)
  
dt <- data.frame(t(data.frame(obsInMEOW)))

dummy <- data.frame(id=c(1:nrow(dt)),polygoon=dt[])
names(dummy) <-c("id","waarde") 
waardes <- sqldf("select waarde as 'polygoon',count(*) as 'aantal' from dummy group by waarde")

 
#Observaties in MEOW
ObsDataInMeow <-data[names(obsInMEOW),]
d <- data.frame(ObsDataInMeow$decimalLongitude,ObsDataInMeow$decimalLatitude)
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)

<<<<<<< HEAD
#Waarnemingen dat binnen meow vallen + dataset connectie meow polygoon ID en waarneming ID

spatialpolygonsListMEOW <- as.SpatialPolygons.PolygonsList(shapeEcoregions@polygons, proj4string=CRS(as.character(NA)))  
obsJoinMEOW<- sp::over(coords,spatialpolygonsListMEOW,returnList = TRUE)
=======
dtf <- data.frame(age=rchisq(100000,10),group=factor(sample(1:10,100000,rep=T)))
 
 obsInMEOW[c(1,2)]
>>>>>>> e9e118c683d25b5f0bca7aa6d53ba301a585272f
