<<<<<<< HEAD
## sample data: bivariate normal, with covariance/correlation 0
x <- data$decimalLongitude
x<- na.omit(x)
y<- data$decimalLatitude
y <- na.omit(y)
## load MASS
library(MASS)

## domain:
xlim <- range(x)
ylim <- range(y)
## 2D Kernel Density Estimation
den <- kde2d(x, y, n = 116, lims = c(xlim, ylim)) #gridpoints=> zoals xlim, mooie range over x
##persp(den$x,den$y,den$z)
z <- den$z  ## extract density
#nieuwe tabel maken met alle mogelijke x,y combinaties en z-waarden => zdata
zdata <- read_excel("C:/Users/bruno/OneDrive/Bureaublad/zdata.xlsx")
test <- na.omit(zdata$z)
sum(test) #2.64 ipv 1 => normaliseren?


##select IDS of grid points that are in LMEs
d <- data.frame(den$x,den$y)
zdata2 <-cbind(zdata$x, zdata$y)
zdata2 <- data.frame(zdata2)
coords <-SpatialPoints(zdata2, proj4string=CRS(as.character(NA)), bbox = NULL)
shapeLME <- mr_shp(
  key = "MarineRegions:lme" ,maxFeatures = 500 
)
spatialpolygonsListLME<- as.SpatialPolygons.PolygonsList(shapeLME@polygons, proj4string=CRS(as.character(NA)))  
obsJoinLME<- sp::over(coords,spatialpolygonsListLME,returnList = TRUE)
obsInLME <-  obsJoinLME[lapply(obsJoinLME,length)>0]
ObsDataInLME <-zdata[names(obsInLME),]
O2 <- data.frame( ID = names(obsInLME), LMEid= unlist(obsInLME), x= ObsDataInLME[, "x" ], y=ObsDataInLME[, "y" ], stringsAsFactors = FALSE)
View(O2)
=======
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

>>>>>>> 5cf88f9be507082bf0d307c36408ccc5d53438b8

## get z-values of these ID's

<<<<<<< HEAD
ID <- 1:13456
zdata <- cbind(ID, zdata)
View(zdata)
library(dplyr)
m<- merge(zdata, O2, by="ID", all.y=TRUE)
View(m)
m <- data.frame(m)

##summarize per LME
x<- xtabs(m$z~m$LMEid)
x
plot(x)
categories <- unique(m$LMEid)
=======
#ID van observaties die overlappen met LME polygonen


# return the number of points in each polygon:
 
# O2 <- over(coords,spatialpolygonsListLME, returnList = TRUE) #ID => z-waarden van opzoeken


coordinates(d) = ~ x + y


## z-value ophalen van coordinaten die opverlappen met polygoon

library(MASS)
f <- kde2d(x,y, h=5, n=20) # met n= grid points
df2 <- do.call(rbind.data.frame, f) # check the z-values of the kde2d function

## daarna nog eens een join met df2 (alle x en y van res moeten gelijk zijn met x en y van df2)
res2 <- res %>% left_join(df2, by = c("ID", )) %>%
  mutate(Values=coalesce(Values.x,Values.y)) %>%
  select(-Values.x,-Values.y)

## tot slot nemen we de som van alle z-waarden van res2

>>>>>>> 5cf88f9be507082bf0d307c36408ccc5d53438b8
