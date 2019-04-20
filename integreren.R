 install.packages("ggtern")

library("igraph")
library("devtools")
library(rgdal)
library(maptools)
library(sp)
library("ggtern")
library('httr')
library('data.table')
library(MASS)
library("worrms")
library(rgeos)
library("mregions")
library("robis")
library('leaflet')
library("sqldf")
library(dplyr)

#waarneming ophalen en omzetten naar spatialpoints

data <- occurrence("Abra alba") 
d <- data.frame(data$decimalLongitude,data$decimalLatitude)
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)

#Waarnemingen dat binnen LME vallen + dataset connectie LME polygoon ID en waarneming ID

spatialpolygonsListLME<- as.SpatialPolygons.PolygonsList(shapeLME@polygons, proj4string=CRS(as.character(NA)))  
obsJoinLME<- sp::over(coords[[1]],spatialpolygonsListLME,returnList = TRUE)
obsInLME <-  obsJoinLME[lapply(obsJoinLME,length)>0]
ObsDataInLME <-data[names(obsInLME),]



O2 <- data.frame( ID = names(obsInLME),
                  LMEid = unlist(obsInLME ),
                    x= ObsDataInLME[,"decimalLongitude" ],
                  y= ObsDataInLME[,"decimalLatitude" ],
                  stringsAsFactors=FALSE
) 



#ID van observaties die overlappen met LME polygonen


# return the number of points in each polygon:
 
# O2 <- over(coords,spatialpolygonsListLME, returnList = TRUE) #ID => z-waarden van opzoeken

names(d) <- c("x","y")
coordinates(d) = ~ x + y


## z-value ophalen van coordinaten die opverlappen met polygoon


f <- kde2d(O2[,"x"],O2[,"y"], h=5, n=20) # met n= grid points
df2 <- do.call(rbind.data.frame, f) # check the z-values of the kde2d function

## daarna nog eens een join met df2 (alle x en y van res moeten gelijk zijn met x en y van df2)
res2 <- res %>% left_join(df2, by = c("ID", )) %>%
  mutate(Values=coalesce(Values.x,Values.y)) %>%
  select(-Values.x,-Values.y)

## tot slot nemen we de som van alle z-waarden van res2

