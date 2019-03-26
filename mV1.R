install_github("iobis/robis")
install.packages("mregions")
install.packages("httr")
install.packages("jsonlite")
install.packages("sf")
install.packages("mapview")
<<<<<<< HEAD
install.packages("gdalUtils")
install.packages("RCassandra")
=======
install.packages("gdalUtils") 
install.packages("rmapshaper")
>>>>>>> a3c4b38d65744033dcac3005d0148931a3d70707
install.packages("leaflet")

library("httr")
library("jsonlite")
library("robis")
library("sf")
library("mapview")
library("mregions")
library("gdalUtils")
library("RCassandra")
library("leaflet")
library("devtools")


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

types <- mr_place_types()
head(places$type)

<<<<<<< HEAD
#Using SF and MAPVIEW package
wfs_request="WFS:http://geo.vliz.be/geoserver/wfs?"
query = list(service = "WFS",
             request = "GetFeature",
             version = "1.1.0",
             typeName = "MarineRegions:Longhurst",
             outputFormat = "json",
             propertyname = "provcode",
             propertyname ="provdescr",
             propertyname ="x",
             propertyname ="y",
             propertyname ="area_m2",
             propertyname ="orig_fid",
             propertyname ="mrgid",
             CQL_FILTER = spiritf(provcode LIKE 'NECS', provdescr LIKE 'Coastal - NE Atlantic Shelves Province' ,x = 5.12586490079 ,
             y = 55.8121098403 , area_m2=2647804822160 , orig_fid = 21 , mrgid=21460))
=======
x <- grep("LONGHURST", types$type,ignore.case = TRUE, value = TRUE)
res <- mr_records_by_type(type=x)
res <- 
# map waarnemingen en polygonen op 1 kaart
data <- occurrence("Abra alba") #Restrict fields in result set
 
shp <- mr_shp(key = "MarineRegions:longhurst", read = TRUE)

map <-leaflet() %>%
      addTiles() %>%
      addMarkers(data=data,lng = ~decimalLongitude,lat = ~decimalLatitude) %>%
      addPolygons(data = shp)
>>>>>>> a3c4b38d65744033dcac3005d0148931a3d70707

map


<<<<<<< HEAD
#change1
#Robis package
library("robis", lib.loc="~/R/win-library/3.5")
data <- occurrence("Abra alba") #Get occurrences by scientific name
data <- occurrence(aphiaid = 141433) #Get occurrences by AphiaID
View(data)
data <- occurrence("Abra alba", fields = c("decimalLongitude", "decimalLatitude")) #Restrict fields in result set
data <- occurrence("Abra nitida", qc = c(22, 23))#Filter occurrences by QC flags
data <- occurrence("Abra alba", geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")#Get occurrences by geometry
map_leaflet(data)#Plot occurrences on a Leaflet map
data <- checklist("Semelidae", year = 2005)
data2 <- checklist(geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
datasets <- dataset(seq(2500, 2520))
=======
#check of waarnemingen in een polygoon liggen
d <- data.frame(data$decimalLatitude,data$decimalLongitude)
d
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
s <- as.SpatialPolygons.PolygonsList(shp@polygons, proj4string=CRS(as.character(NA)))
leaflet() %>%
  addTiles() %>%
addPolygons(data = shp)
a <- sp::over(coords,s@polygons)
sum(a,na.rm = TRUE)
 
>>>>>>> a3c4b38d65744033dcac3005d0148931a3d70707
