library("aquamapsdata")
library("purrr")

library("igraph")
library("devtools")
library(rgdal)
library(maptools)
library(sp)

library('httr')
library('data.table')

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
obsJoinLME<- sp::over(coords,spatialpolygonsListLME,returnList = TRUE)
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
 

 

coordinates(d) = ~ x + y




## eerst een join met de ids van de observaties die vallen in de polygonen en de ids van de dataset d waar de x en y coordinaten staan

res <- d %>% left_join(w, by = c("ID" )) %>%
  mutate(Values=coalesce(Values.x,Values.y)) %>%
  select(-Values.x,-Values.y)


## z-value ophalen van coordinaten die opverlappen met polygoon

library(MASS)
f <- kde2d(x,y, h=5, n=20) # met n= grid points
df2 <- do.call(rbind.data.frame, f) # check the z-values of the kde2d function

## daarna nog eens een join met df2 (alle x en y van res moeten gelijk zijn met x en y van df2)
res2 <- res %>% left_join(df2, by = c("ID", )) %>%
  mutate(Values=coalesce(Values.x,Values.y)) %>%
  select(-Values.x,-Values.y)

## tot slot nemen we de som van alle z-waarden van res2

