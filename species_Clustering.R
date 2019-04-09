install.packages("proxy")
install.packages("jsonlite")
install.packages("stringr")

library('httr')
library('data.table')
library(rgeos)
library("robis")


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

names(d) <- c("x","y")
summary(d)

#get gazetteer data of variable
distribution <- wm_distribution(wm_name2id("Abra alba"))

MGRID <- str_split(distribution[2,]$locationID,"/",simplify = TRUE)
MGRID[5]
response <- GET(url = paste("http://www.marineregions.org/rest/getGazetteerWMSes.json/",MGRID[5],"/",sep="") )
paste("http://www.marineregions.org/rest/getGazetteerWMSes.json/",MGRID[5],"/",sep="")
record2 <- content(response)
 


http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS
&version=1.0.0&request=GetFeature
&typeName=MarineRegions:iho&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eid%3C/
  PropertyName%3E%3CLiteral%3E1a%3C/Literal%3E%3C/PropertyIsEqualTo%3E


shapeprobeer <- mr_shp(
  key = paste(record2[[1]]$namespace,record2[[1]]$featureType, sep = ":"),  maxFeatures = 300
)
shapeprobeer <- mr_shp(
  key = "MarineRegions:eez", filter = "Belgian Exclusive Economic Zone", maxFeatures = 300
)


#calculate distance
dist_d <- dist(d[1:30000,],method='euclidean')
sapply(d, function(x) sum(is.na(x)))


#plotting
leaflet() %>%
  addTiles()%>%
  addPolygons(data = shapeprobeer)  %>%
  addPolygons(data = shapeprobeer2,color = "green") 
