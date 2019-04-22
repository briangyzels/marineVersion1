
library("leaflet")
library("mregions")
library("robis")

shapeEEZ  <- mr_shp(
  key = "MarineRegions:eez" ,maxFeatures = 500 
) 
shapeEcoregions <- mr_shp(
  key= "Ecoregions:ecoregions" ,maxFeatures = 500 
) 
shapeLME <- mr_shp(
  key = "MarineRegions:lme" ,maxFeatures = 500 
)
shapeLME <-shapeLME[order(shapeLME@data$lme_number),]

specieses <- c("Abra Alba","Pleuronectes platessa","Phoca vitulina","Aurelia aurita","Dermochelys coriacea")
coords = list()
for (a in 1:5){
  data <- occurrence(specieses[a]) 
  d <- data.frame(data$decimalLongitude,data$decimalLatitude)
  coords[a] <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
}
 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = b[c(22),])
