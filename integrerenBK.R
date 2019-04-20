
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

## get z-values of these ID's

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
