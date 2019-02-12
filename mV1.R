install_github("iobis/robis")
install.packages("mregions")
install.packages("httr")
install.packages("jsonlite")
install.packages("sf")
install.packages("mapview")
install.packages("gdalUtils") 
install.packages("rmapshaper")
install.packages("leaflet")

library("httr")
library("jsonlite")
library("robis")
library("sf")
library("mapview")
library("mregions")
library("gdalUtils")


#change1
#Robis package
library("robis", lib.loc="~/R/win-library/3.5")
library("leaflet")
# data <- occurrence("Abra alba") #Get occurrences by scientific name
# data <- occurrence(aphiaid = 141433) #Get occurrences by AphiaID
# View(data)
# data <- occurrence("Abra alba", fields = c("decimalLongitude", "decimalLatitude")) #Restrict fields in result set
# data <- occurrence("Abra nitida", qc = c(22, 23))#Filter occurrences by QC flags
# data <- occurrence("Abra alba", geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")#Get occurrences by geometry
# head(data)
# leafletmap(data)#Plot occurrences on a Leaflet map
# data <- checklist("Semelidae", year = 2005)
# data2 <- checklist(geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
# datasets <- dataset(seq(2500, 2520))

res1 <- mr_records_by_type(type = "EEZ")

# map waarnemingen en polygonen op 1 kaart
data <- occurrence("Abra alba") #Restrict fields in result set
 
shp <- mr_shp(key = "MarineRegions:EEZ", maxFeatures = 5)

map <-leaflet() %>%
      addTiles() %>%
      addMarkers(data=data,lng = ~decimalLongitude,lat = ~decimalLatitude) %>%
      addPolygons(data = shp)

map


#check of waarnemingen in een polygoon liggen
d <- data.frame(data$decimalLatitude,data$decimalLongitude)
d
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
s <- as.SpatialPolygons.PolygonsList(shp@polygons[[1]], proj4string=CRS(as.character(NA)))

a <- sp::over(coords,s)
sum(a,na.rm = TRUE)
 
