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
shapeLME <- mr_shp(
  key = "MarineRegions:lme" ,maxFeatures = 500 
)
 
#get gazetteer data of variable
distribution <- wm_distribution(wm_name2id("Abra alba"))
distinct_distribution <-sqldf("select distinct(locationID),locality from distribution")

LMEdata   <- data.frame( objectID = numeric(),
                           NumberObsNear = numeric(),
                           NumberOfObs= numeric(),
                           Size = numeric(),
                           stringsAsFactors=FALSE
) 

 
gazetteerPolygonDF<- data.frame( MGRID=character(),
                  GazetteerName=character(),
                NumberObsNear = numeric(),
                 NumberOfObs= numeric(),
                 Size = numeric(),stringsAsFactors=FALSE
                 ) 
  
for (i in 1:length(shapeLME)){
# MGRID <- str_split(distinct_distribution[i,]$locationID,"/",simplify = TRUE)
# response <- GET(url= "http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MarineRegions:iho&filter=<PropertyIsEqualTo><PropertyName>id</PropertyName><Literal>1a</Literal></PropertyIsEqualTo>" )
# record <- content(response)
# i
# shapeprobeer <- mr_shp(
#   key = paste(record[[1]]$namespace,record[[1]]$featureType, sep = ":"),  maxFeatures = 500, read = TRUE
# )
# filterd <-shapeprobeer[toupper(shapeprobeer@data[,record[[1]]$featureName]) == toupper(record[[1]]$value),]
#  

LMEdata[i,]$objectID <- i
 LMEdata[i,]$Size <- gArea(shapeLME[i,])
#gazetteerPolygonDF.MGRID
# gazetteerPolygonDF[i,]$MGRID <- MGRID[5]
# 
# #gazetteerPolygonDF.
# gazetteerPolygonDF[i,]$GazetteerName <- distinct_distribution[i,]$locality
 
# filterd@polygons
# 
#gazetteerPolygonDF.numberOfOBs
 listObsInPolygon <- sp::over(coords, as.SpatialPolygons.PolygonsList(shapeLME[i,]@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
 LMEdata[i,]$NumberOfObs <- sum(lapply(listObsInPolygon,length)>0)

#LMEdata.numberofOBSNear
 if (!i ==35){
 neighboursIDs <-gTouches(shapeLME[i,],shapeLME, byid=TRUE,returnDense = FALSE)
 
 if (length(neighboursIDs[[1]])>0){
   neighbours<-shapeLME[neighboursIDs[[1]],] 
 
  listObsInPolygonNear <- sp::over(coords,as.SpatialPolygons.PolygonsList(neighbours@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
LMEdata[i,]$NumberObsNear <- sum(lapply(listObsInPolygonNear,length)>0)
   
 }else {
   LMEdata[i,]$NumberObsNear <- 0
   
 }

 
 }
 else{
   LMEdata[i,]$NumberObsNear <- 0
 }
}
# 
# #gazetteerPolygonDF.Size
# gazetteerPolygonDF[i,]$Size <- sizeGazetteerRegion
# 
#  sum <- sum + 1

# shapeprobeer2 <- mr_shp(
#   key = "MarineRegions:eez" ,maxFeatures = 500 
# ) 
 i
dummy <- data.frame (LMEdata[,2:4])
scaled_dummy <-scale(dummy)
dist_scaled_dummy <- dist(scaled_dummy)
hc <- hclust(dist_scaled_dummy,method = "complete")
# filterd <- shapeprobeer3[shapeprobeer3$polygon_nm == "OOSTENDE", ]
# names(shapeprobeer3@data)
ggplot(lineup_k2_complete, aes(x = x, y = y, color = factor(cluster))) +
  geom_point()        

plot(hc)
#plotting
leaflet() %>%
  addTiles() %>%
  addPolygons(data = shapeLME )  %>%
  addPolygons(data = shapeLME[23,],color = "green")

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
