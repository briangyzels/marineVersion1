install.packages("proxy")
install.packages("jsonlite")
install.packages("stringr")
install.packages("raster")
library('httr')
library('data.table')

library("robis")
library("maptools")
library(rgeos)
library('leaflet')
library("httr")
library("jsonlite")
library("sqldf")
library("worrms")
library("mregions")
library("proxy")
library("dplyr")
library("curl")
library("stringr")
 
 sum <- 0

shapeEEZ  <- mr_shp(
   key = "MarineRegions:eez" ,maxFeatures = 500 
 ) 
shapeEcoregions <- mr_shp(
  key= "Ecoregions:ecoregions" ,maxFeatures = 500 
) 
ShapeLME <- mr_shp(
  key = "MarineRegions:lme" ,maxFeatures = 500 
)
 
#get gazetteer data of variable
distribution <- wm_distribution(wm_name2id("Abra alba"))
distinct_distribution <-sqldf("select distinct(locationID),locality from distribution")
 
 
gazetteerPolygonDF<- data.frame( MGRID=character(),
                  GazetteerName=character(),
                 OverlappingMEOW=numeric(),
                 OverlappingLME=numeric(),
                 OverlappingEEZ = numeric(),
                 NumberOfObs= numeric(),
                 Size = numeric(),stringsAsFactors=FALSE
                 ) 
  
for (i in 1:3){
MGRID <- str_split(distinct_distribution[i,]$locationID,"/",simplify = TRUE)
response <- GET(url = paste("http://www.marineregions.org/rest/getGazetteerWMSes.json/",MGRID[5],"/",sep="") )
record <- content(response)
i
shapeprobeer <- mr_shp(
  key = paste(record[[1]]$namespace,record[[1]]$featureType, sep = ":"),  maxFeatures = 500, read = TRUE
)
filterd <-shapeprobeer[toupper(shapeprobeer@data[,record[[1]]$featureName]) == toupper(record[[1]]$value),]
 
sizeGazetteerRegion <- gArea(filterd)
#gazetteerPolygonDF.MGRID
gazetteerPolygonDF[i,]$MGRID <- MGRID[5]

#gazetteerPolygonDF.
gazetteerPolygonDF[i,]$GazetteerName <- distinct_distribution[i,]$locality

#gazetteerPolygonDF.OverlappingMEOW
intersectMEOW <- gIntersection(filterd,shapeEcoregions)
gazetteerPolygonDF[i,]$OverlappingMEOW <- round(gArea(intersectMEOW)/sizeGazetteerRegion , 2 )

#gazetteerPolygonDF.OverlappingLME
intersectLME <- gIntersection(filterd,ShapeLME)
gazetteerPolygonDF[i,]$OverlappingLME <- round(gArea(intersectLME)/sizeGazetteerRegion , 2 )

#gazetteerPolygonDF.OverlappingEEZ
intersectEEZ <- gIntersection(filterd,shapeEEZ)
gazetteerPolygonDF[i,]$OverlappingEEZ <- round(gArea(intersectEEZ)/sizeGazetteerRegion , 2 )

#gazetteerPolygonDF.numberOfOBs
listObsInPolygon <- sp::over(coords,as.SpatialPolygons.PolygonsList(filterd@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
gazetteerPolygonDF[i,]$NumberOfObs <- sum(lapply(listObsInPolygon,length)>0)

#gazetteerPolygonDF.Size
gazetteerPolygonDF[i,]$Size <- sizeGazetteerRegion

 sum <- sum + 1
}
# shapeprobeer2 <- mr_shp(
#   key = "MarineRegions:eez" ,maxFeatures = 500 
# ) 
 
 
# filterd <- shapeprobeer3[shapeprobeer3$polygon_nm == "OOSTENDE", ]
# names(shapeprobeer3@data)
          
#plotting
leaflet() %>%
  addTiles()%>%
  addPolygons(data = filterd)  %>%
  addPolygons(data = shapeprobeer2,color = "green")

#calculate distance
dist_d <- dist(d[1:30000,],method='euclidean')
sapply(d, function(x) sum(is.na(x)))

#GetShapeFilesOfGazetteer
 

for (i in 1:5){
  MGRID <- str_split(distinct_distribution[i,]$locationID,"/",simplify = TRUE)
  response <- GET(url = paste("http://www.marineregions.org/rest/getGazetteerWMSes.json/",MGRID[5],"/",sep="") )
  record <- content(response)
  i
  shapeprobeer <- mr_shp(
    key = paste(record[[1]]$namespace,record[[1]]$featureType, sep = ":"),  maxFeatures = 500
  )
  if (i>1){
    newGazetteerPolygon  <-shapeprobeer[toupper(shapeprobeer@data[,record[[1]]$featureName]) == toupper(record[[1]]$value),]
    # newGazetteerPolygon <- spChFIDs(newGazetteerPolygon, paste("b", row.names(newGazetteerPolygon), sep="."))
    gazetteerPolygons@polygons <-   c(gazetteerPolygons@polygons,newGazetteerPolygon@polygons)
     
    # gazetteerPolygons <- spRbind(gazetteerPolygons,newGazetteerPolygon)
  }else
  {
       gazetteerPolygons <-shapeprobeer[toupper(shapeprobeer@data[,record[[1]]$featureName]) == toupper(record[[1]]$value),]
 
  }
  
  }
#plotting gazetteer
leaflet() %>%
  addTiles()%>%
  addPolygons(data = gazetteerPolygons)   
