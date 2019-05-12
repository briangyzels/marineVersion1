install.packages("neuralnet")

library("sp")
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
library(readxl)
library("neuralnet")
 
 
specieses <- c("Abra Alba","Pleuronectes platessa","Phoca vitulina","Aurelia aurita","Dermochelys coriacea")
coords = list()
for (a in 1:5){
  data <- occurrence(specieses[a]) 
  d <- data.frame(data$decimalLongitude,data$decimalLatitude)
  coords[a] <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
}
LMETrainingset   <- data.frame( LMEid = numeric(),
                                Species = character(),
                         NumberObsNear = numeric(),
                         NumberOfObs= numeric(),
                         Size = numeric(),
                         InPolygon = logical(),
                      stringsAsFactors=FALSE
)   
for (j in 4:5){

for (i in 1:length(shapeLME)){
 
  LMETrainingset[i+(j-1)*66,]$LMEid <- i



  LMETrainingset [i+(j-1)*66,]$Size <- gArea(shapeLME[i,])

  #numberOfOBs
  listObsInPolygon <- sp::over(coords[[j]], as.SpatialPolygons.PolygonsList(shapeLME[i,]@polygons, proj4string=CRS(as.character(NA))),returnList = TRUE)
  LMETrainingset [i+(j-1)*66,]$NumberOfObs <- sum(lapply(listObsInPolygon,length)>0)
  
  #numberofOBSNear
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
# LMETrainingset[,c(3,4,5)] <- scale(LMETrainingset[,c(3,4,5)])
LMETrainingset[1:66,]$InPolygon <-c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
LMETrainingset[67:132,]$InPolygon <-c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
LMETrainingset[133:198,]$InPolygon <-c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE)
LMETrainingset[199:264,]$InPolygon <-c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
LMETrainingset[265:330,]$InPolygon <- c(TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,FALSE, TRUE,TRUE,TRUE, TRUE,FALSE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,TRUE, TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE,TRUE, FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)

nn=neuralnet(InPolygon~NumberObsNear+NumberOfObs+Size,data=LMETrainingset[0:330,] ,act.fct = "logistic",
             linear.output = FALSE,hidden=c(3,2),stepmax=1e7,threshold = 0.01)
plot(nn)

#test set
LMETest <- data.frame( LMEid = numeric(), 
                                NumberObsNear = numeric(),
                                NumberOfObs= numeric(),
                                Size = numeric(),
                                stringsAsFactors=FALSE
) 
dataTest <- occurrence("Clupea harengus")  
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

# cols <- c(2,3,4)
# LMETest_scaled <- LMETest[]
# LMETest_scaled[,cols] <- scale(LMETest[,cols])

# LMETrainingset[331:396,]$NumberObsNear <- LMETest$NumberObsNear
# LMETrainingset[331:396,]$NumberOfObs <- LMETest$NumberOfObs
# LMETrainingset[331:396,]$Size <- LMETest$Size
#  
# scaled_set <-LMETrainingset[]
#   cols <- c(3,4,5)
#   LMETest_scaled <- LMETest
#   scaled_set[,cols] <- scale(LMETrainingset[,cols])

#predicie + vergelijking met enkel op observaties
Predict=compute(nn,LMETest)
prob <- Predict$net.result
resultset <-lme_aqm3[,'Clupea harengus']
results <- data.frame(actual = resultset, prediction = ifelse(prob>0.5, 1, 0),VLIZ = ifelse(LMETest$NumberOfObs>0,1,0))

names(results) = c("actual","prediction","VLIZ")
 
 table(actual = results$actual,prediction = results$prediction)
 table(actual = results$actual,VLIZ = results$VLIZ)
 
lme_aqm3 <- read_excel("C:/Users/Brian/Google Drive/Thesis/PVA/lme_aqm3.xlsx")
