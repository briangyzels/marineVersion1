

#ID van observaties die overlappen met LME polygonen


d <- data.frame(ObsDataInLME$decimalLongitude,ObsDataInLME$decimalLatitude)
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)


#Waarnemingen dat binnen LME vallen + dataset connectie LME polygoon ID en waarneming ID

spatialpolygonsListLME<- as.SpatialPolygons.PolygonsList(shp@polygons, proj4string=CRS(as.character(NA)))  
obsJoinLME<- sp::over(coords,spatialpolygonsListLME,returnList = TRUE)
obsInLME <-  obsJoinLME[lapply(obsJoinLME,length)>0]
ObsDataInLME <-data[names(obsInLME),]

coordinates(d) = ~ObsdecimalLongitude+decimalLatitude


# return the number of points in each polygon:
sapply(over(spatialpolygonsListLME, geometry(data), returnList = TRUE), length)


O1<- over(spatialpolygonsListLME, data, returnList = TRUE)
O2 <- over(spatialpolygonsListLME, geometry(data), returnList = TRUE) #ID => z-waarden van opzoeken
O3 <- over(data, spatialpolygonsListLME)
O4<- over(data, shp)

w<- O2[["0"]]
colnames(d)[2] <- "x"
colnames(d)[3] <- "y"

## eerst een join met de ids van de observaties die vallen in de polygonen en de ids van de dataset d waar de x en y coordinaten staan
library(dplyr)
res <- d %>% left_join(w, by = c("ID", )) %>%
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

