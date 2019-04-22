
data <- occurrence("Abra alba")
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
den <- kde2d(x, y ,n = 116, lims = c(xlim, ylim)) #gridpoints=> zoals xlim, mooie range over x
##persp(den$x,den$y,den$z)
z <- den$z  ## extract density
den2 <- data.frame(den)
den2<- t(den2)
write.csv(den2, "den2.csv")
#nieuwe tabel maken met alle mogelijke x,y combinaties en z-waarden => zdata
zdata <- read_excel("C:/Users/bruno/OneDrive/Bureaublad/zdata.xlsx", col_types = c("numeric", "numeric", "numeric"))
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
shapeLME<- shapeLME[order(shapeLME@data$lme_number), ]
spatialpolygonsListLME<- as.SpatialPolygons.PolygonsList(shapeLME@polygons, proj4string=CRS(as.character(NA)))  
obsJoinLME<- sp::over(coords,spatialpolygonsListLME,returnList = TRUE)
obsInLME <-  obsJoinLME[lapply(obsJoinLME,length)>0]
ObsDataInLME <-zdata[names(obsInLME),]
O2 <- data.frame( ID = names(obsInLME), LMEid= unlist(obsInLME), x= ObsDataInLME[, "x" ], y=ObsDataInLME[, "y" ], stringsAsFactors = FALSE)
View(O2)

## get z-values of these ID's

ID <- 1:13456
zdata<- cbind(ID, zdata)
View(zdata)
library(dplyr)
m<- merge(zdata, O2, by="ID", all.y=TRUE)
View(m)
m <- data.frame(m)

##summarize per LME
XT1<- xtabs(m$z~m$LMEid)
XT1
plot(XT1)
categories <- unique(m$LMEid)

##Same for MEOW
coords <-SpatialPoints(zdata2, proj4string=CRS(as.character(NA)), bbox = NULL)
shapeMEOW <- mr_shp(key= "Ecoregions:ecoregions",maxFeatures = 500 )
shapeMEOW<- shapeMEOW[order(shapeMEOW@data$eco_code), ]
spatialpolygonsListMEOW<- as.SpatialPolygons.PolygonsList(shapeMEOW@polygons, proj4string=CRS(as.character(NA)))  
obsJoinMEOW<- sp::over(coords,spatialpolygonsListMEOW,returnList = TRUE)
obsInMEOW <-  obsJoinMEOW[lapply(obsJoinMEOW,length)>0]
ObsDataInMEOW <-zdata[names(obsInMEOW),]
O2 <- data.frame( ID = names(obsInMEOW), MEOWid= unlist(obsInMEOW), x= ObsDataInMEOW[, "x" ], y=ObsDataInMEOW[, "y" ], stringsAsFactors = FALSE)
View(O2)
m<- merge(zdata, O2, by="ID", all.y=TRUE)
View(m)
m <- data.frame(m)
x<- xtabs(m$z~m$MEOWid)
x
ABALKLME<- plot(x)

##Aquamaps validation
x <- ABAL$Long
x<- na.omit(x)
y<- ABAL$Lat
y <- na.omit(y)
xlim <- range(x)
ylim <- range(y)
den <- kde2d(x, y, h=0.25, n = 116, lims = c(xlim, ylim))
z <- den$z  
den2 <- data.frame(den)
den2 <-t(den2)
den2<- data.frame(den2)
write.csv(den2, "aqmden2.csv")
zdataAQM <- read_excel("zdataAQM.xlsx", col_types = c("numeric","numeric", "numeric"))
test <- na.omit(zdataAQM$z)
sum(test) #6.5448 ipv 1 => normaliseren?
zdataAQM2 <-cbind(zdataAQM$x, zdataAQM$y)
zdataAQM2 <- data.frame(zdataAQM2)

coords <-SpatialPoints(zdataAQM2, proj4string=CRS(as.character(NA)), bbox = NULL)
shapeLME <- mr_shp(
  key = "MarineRegions:lme" ,maxFeatures = 500 
)
shapeLME<- shapeLME[order(shapeLME@data$lme_number), ]
spatialpolygonsListLME<- as.SpatialPolygons.PolygonsList(shapeLME@polygons, proj4string=CRS(as.character(NA)))  
obsJoinLME<- sp::over(coords,spatialpolygonsListLME,returnList = TRUE)
obsInLME <-  obsJoinLME[lapply(obsJoinLME,length)>0]
ObsDataInLME <-zdataAQM[names(obsInLME),]
O2 <- data.frame( ID = names(obsInLME), LMEid= unlist(obsInLME), x= ObsDataInLME[, "x" ], y=ObsDataInLME[, "y" ], stringsAsFactors = FALSE)
View(O2)

ID <- 1:13456
zdataAQM <- cbind(ID, zdataAQM)
View(zdataAQM)
maq<- merge(zdataAQM, O2, by="ID", all.y=TRUE)
View(maq)
maq <- data.frame(maq)
XT2<- xtabs(maq$z~maq$LMEid)
XT2
par(mfrow=c(1,2))
barplot(XT1)
barplot(XT2)
plot1<- ggplot(m, aes(m$LMEid, m$z))+geom_bar(stat = "identity", aes(fill = m$z))+ theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
plot2<- ggplot(maq, aes(maq$LMEid, maq$z))+geom_bar(stat = "identity", aes(fill = maq$z))+ theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

## Andere soort

data <- occurrence("Abra alba")
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
den2 <- data.frame(den)