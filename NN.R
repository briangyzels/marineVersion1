install.packages("neuralnet")

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


library("neuralnet")
 
LMETrainingset   <- data.frame( LMEid = numeric(),
                                Species = character(),
                         NumberObsNear = numeric(),
                         NumberOfObs= numeric(),
                         Size = numeric(),
                         InPolygon = logical(),
                         stringsAsFactors=FALSE
) 
specieses <- c("Abra Alba","Pleuronectes platessa","Phoca vitulina","Aurelia aurita","Dermochelys coriacea")
coords = list()
for (a in 1:5){
  data <- occurrence(specieses[a]) 
  d <- data.frame(data$decimalLongitude,data$decimalLatitude)
  coords[a] <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
}

for (j in 3:5){

for (i in 1:length(shapeLME)){
 
  LMETrainingset[i+(j-1)*66,]$LMEid <- i



  LMETrainingset [i+(j-1)*66,]$Size <- gArea(shapeLME[i,])

  #gazetteerPolygonDF.numberOfOBs
  listObsInPolygon <- sp::over(coords[[j]], as.SpatialPolygons.PolygonsList(shapeLME[i,]@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
  LMETrainingset [i+(j-1)*66,]$NumberOfObs <- sum(lapply(listObsInPolygon,length)>0)
  
  #LMEdata.numberofOBSNear
  if (!i ==35){
    neighboursIDs <-gTouches(shapeLME[i,],shapeLME, byid=TRUE,returnDense = FALSE)
    
    if (length(neighboursIDs[[1]])>0){
      neighbours<-shapeLME[neighboursIDs[[1]],] 
      
      listObsInPolygonNear <- sp::over(coords[[j]],as.SpatialPolygons.PolygonsList(neighbours@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
      LMETrainingset [i+(j-1)*66,]$NumberObsNear <- sum(lapply(listObsInPolygonNear,length)>0)
      
    }else {
      LMETrainingset [i+(j-1)*66,]$NumberObsNear <- 0
      
    }
    
    
  }
  else{
    LMETrainingset[i+(j-1)*66,]$NumberObsNear <- 0
  }
}
LMETrainingset[(1+(j-1)*66):(66+(j-1)*66),]$Species <-  specieses[j]
}

LMETrainingset[1:66,]$InPolygon <-c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
LMETrainingset[67:132,]$InPolygon <-c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
LMETrainingset[133:198,]$InPolygon <-c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE)
LMETrainingset[199:264,]$InPolygon <-c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
LMETrainingset[265:330,]$InPolygon <- c(TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,FALSE, TRUE,TRUE,TRUE, TRUE,FALSE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE,TRUE, FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)

nn=neuralnet(InPolygon~NumberObsNear+NumberOfObs+Size,data=LMETrainingset, hidden=4,act.fct = "logistic",
             linear.output = FALSE)
plot(nn)

#test set
LMETest <- data.frame( LMEid = numeric(),
                                
                                NumberObsNear = numeric(),
                                NumberOfObs= numeric(),
                                Size = numeric(),
                                stringsAsFactors=FALSE
) 
dataTest <- occurrence("Manta birostris") 
dtest <- data.frame(dataTest$decimalLongitude,dataTest$decimalLatitude)
coordstest <-SpatialPoints(dtest, proj4string=CRS(as.character(NA)), bbox = NULL)

for (i in 1:length(shapeLME)){
  
  LMETest[i,]$LMEid <- i
  
  
  
  LMETest [i,]$Size <- gArea(shapeLME[i,])
  
  #gazetteerPolygonDF.numberOfOBs
  listObsInPolygon <- sp::over(coordstest, as.SpatialPolygons.PolygonsList(shapeLME[i,]@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
  LMETest [i,]$NumberOfObs <- sum(lapply(listObsInPolygon,length)>0)
  
  #LMEdata.numberofOBSNear
  if (!i ==35){
    neighboursIDs <-gTouches(shapeLME[i,],shapeLME, byid=TRUE,returnDense = FALSE)
    
    if (length(neighboursIDs[[1]])>0){
      neighbours<-shapeLME[neighboursIDs[[1]],] 
      
      listObsInPolygonNear <- sp::over(coordstest,as.SpatialPolygons.PolygonsList(neighbours@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
      LMETest[i,]$NumberObsNear <- sum(lapply(listObsInPolygonNear,length)>0)
      
    }else {
      LMETest [i,]$NumberObsNear <- 0
      
    }
    
    
  }
  else{
    LMETest[i,]$NumberObsNear <- 0
  }
}

Predict=compute(nn,LMETest)
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
sum(pred>0)
