install.packages("igraph")
install_github("raquamaps/aquamapsdata")
install.packages("data.table") 
install.packages("devtools")
install_github("ropenscilabs/mregions")
install.packages("curl")
library("aquamapsdata")
library("purrr")
library("worrms")
library("igraph")
library("devtools")
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library('httr')
library('data.table')
library("mregions")
library("robis")
library('leaflet')

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
#ophalen een specifiek gebied gazetteer
shapefilter <- mr_shp(
  key = "MarineRegions:eez",
  filter ="Dutch Exclusive Economic Zone",  maxFeatures = 300
)

spatialpolygonsList <- as.SpatialPolygons.PolygonsList(shapefilter@polygons, proj4string=CRS(as.character(NA)))  
pointsWithPolygonsJoin <- sp::over(coords,spatialpolygonsList)
fit <- round(x=length(pointsWithPolygonsJoin[complete.cases(pointsWithPolygonsJoin)]) / length(coords),2)


#plot met polygonen met MEOW polygonen in het groen, een polygoon van de gazetteer in het blauw en waarnemingen in het rood
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = coords, radius = 3.5, weight = 0, fillOpacity = 1, fillColor = "#cc3300") %>%
  addPolygons(data = shapefilter) %>%
  addPolygons(data = shapeEcoregions,color = "green") 

lat = data$decimalLatitude, lng = data$decimalLongitude
#MEOW, LME, ... polygonen selecteren die overlappen met gebied uit gazetteer

#Hoe groot is gazetteer gebied
 wkt <- mr_as_wkt(shapefilter)
 
sizeGazetteerRegion <- gArea(readWKT(wkt))

#http://www.imachordata.com/meow-its-marine-ecoregions-in-r-2/ 


mr_names("MarineRegions:eez")
# get Samoan Exclusive Economic Zone

 
res <-subset(res,res$geoname == "Dutch Exclusive Economic Zone") 
rr <- mr_shp(key = "MarineRegions:eez_iho_union_v2",
             filter = "North Atlantic Ocean")
sp::plot(res) 
mr_names_search("EEZ", "Dutch Exclusive Economic Zone")
#85668 mgrid



