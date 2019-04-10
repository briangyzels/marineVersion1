install.packages("proxy")
install.packages("jsonlite")
install.packages("stringr")

library('httr')
library('data.table')

library("robis")

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
source("ShapeFunction.R")
names(d) <- c("x","y")
summary(d)

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
wm_record(id = 105706)
gazetteerPolygonDF<- data.frame( MGRID=character(),
                  GazetteerName=character(),
                 OverlappingMEOW=numeric(),
                 OverlappingLME=numeric(),
                 OverlappingEEZ = numeric(),
                 NumberOfObs= numeric(),
                 Size = numeric()
                 ) 
  
for (i in 3:3){
MGRID <- str_split(distinct_distribution[i,]$locationID,"/",simplify = TRUE)
response <- GET(url = paste("http://www.marineregions.org/rest/getGazetteerWMSes.json/",MGRID[5],"/",sep="") )
record <- content(response)

shapeprobeer <- mr_shp(
  key = paste(record[[1]]$namespace,record[[1]]$featureType, sep = ":"),  maxFeatures = 500, read = TRUE
)
 
filterd <-shapeprobeer[toupper(shapeprobeer@data$polygon_nm) == toupper(distinct_distribution[i,]$locality),]


#gazetteerPolygonDF.MGRID
gazetteerPolygonDF[i,]$MGRID = MGRID[5]

#gazetteerPolygonDF.
gazetteerPolygonDF[i,]$GazetteerName = distinct_distribution[i,]$locality

#gazetteerPolygonDF.OverlappingMEOW
intersectMEOW <- gIntersection(filterd,shapeEcoregions)
gazetteerPolygonDF[i,]$OverlappingMEOW <- round(gArea(intersectMEOW)/sizeGazetteerRegion , 2 )

#gazetteerPolygonDF.OverlappingLME
intersectLME <- gIntersection(filterd,shapeEcoregions)
gazetteerPolygonDF[i,]$OverlappingLME <- round(gArea(intersectLME)/sizeGazetteerRegion , 2 )

#gazetteerPolygonDF.OverlappingEEZ
intersectEEZ <- gIntersection(filterd,shapeEcoregions)
gazetteerPolygonDF[i,]$OverlappingEEZ <- round(gArea(intersectEEZ)/sizeGazetteerRegion , 2 )

#gazetteerPolygonDF.numberOfOBs
listObsInPolygon <- sp::over(coords,as.SpatialPolygons.PolygonsList(filterd@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
gazetteerPolygonDF[i,]$NumberOfObs <- sum(lapply(listObsInPolygon,length)>0)

#gazetteerPolygonDF.Size
gazetteerPolygonDF[i,]$Size <- gArea(filterd)

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

