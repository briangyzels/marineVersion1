install.packages("igraph")
library("devtools")
install_github("raquamaps/aquamapsdata")
library(aquamapsdata)
install.packages("data.table") 
 
library("worrms")

library("aquamapsdata")
library("purrr")

library("igraph")
library("devtools")
library("rgdal")
library("maptools")
library("rgeos")
library("dplyr")
library('httr')
library('data.table')
library("mregions")
library("robis")
library('leaflet')



ABRALBADATA <- wm_distribution_(141433)
types <- mr_place_types()
reg<- mr_records_by_type(type = "EEZ")
reg2<- mr_records_by_type(type = "EEZ", offset=100)
reg3<- mr_records_by_type(type = "EEZ", offset=200)
meow<- mr_records_by_type(type = "Marine Ecoregion of the World (MEOW)")
meow2<- mr_records_by_type(type = "Marine Ecoregion of the World (MEOW)", offset=100)
meow3<- mr_records_by_type(type = "Marine Ecoregion of the World (MEOW)", offset=200)

longhurst<- mr_records_by_type(type = "Longhurst Province")
town<- mr_records_by_type(type = "Town") 
town2<- mr_records_by_type(type = "Arrondissement", offset=100)
town3<- mr_records_by_type(type = "Town", offset=200)
town4<- mr_records_by_type(type = "Town", offset=300)
town5<- mr_records_by_type(type = "Town", offset=400) 
town6<- mr_records_by_type(type = "Town", offset=500) 
town7<- mr_records_by_type(type = "Town", offset=600)
town8<- mr_records_by_type(type = "Town", offset=700) 
town9<- mr_records_by_type(type = "Town", offset=800) 
town10<- mr_records_by_type(type = "Town", offset=900) #belgische badplaatsen te gebruiken
gulf<- mr_records_by_type(type = "Gulf")
gulf2<- mr_records_by_type(type = "Gulf", offset=100)
sea<- mr_records_by_type(type = "Sea")

#39 plaatsen zoeken in deze en plotten op kaart
#https://recology.info/2016/06/marine-regions/
res <- mr_shp(key= "MarineRegions:eez_", maxFeatures=200)
View(wkt)
wkt <- mr_as_wkt(shp)
gArea(readWKT(wkt))
args <- list(scientificname = "Abra alba", geometry = wkt, limit = 100)
res <- httr::GET('http://api.iobis.org/occurrence', query = args)

xx <- data.table::setDF(data.table::rbindlist(httr::content(res)$results, use.names = TRUE, fill = TRUE))
 
#ophalen van waarneming doormiddel van robis pakket en vervolgens spatialpoint van te maken
data <- occurrence("Abra alba") 
d <- data.frame(data$decimalLatitude,data$decimalLongitude)
coords <-SpatialPoints(d, proj4string=CRS(as.character(NA)), bbox = NULL)
shp <- mr_shp(key= "Ecoregions:ecoregions", maxFeatures = 1000)

xx <- xx[, c('scientificName', 'decimalLongitude', 'decimalLatitude')]
names(xx)[2:3] <- c('longitude', 'latitude')

#plot met polygonen met polygonen in het blauw en waarnemingen in het rood
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = coords,color = "red") %>%
  addPolygons(data = shp)

#MEOW, LME, ... polygonen selecteren die overlappen met gebied uit gazetteer
shp <- mr_geojson(name= "meow") 
res<- mr_geojson(key= "Ecoregions:ecoregions") #MEOW
#Brian: zo haal je MEOW op
shp <- mr_shp(key= "Ecoregions:ecoregions")#dit argument lijkt niet te bestaan? 
#http://www.imachordata.com/meow-its-marine-ecoregions-in-r-2/ 


#nog nodig?
#https://databasin.org/datasets/3b6b12e7bcca419990c9081c0af254a2 download shapefiles in zip bestand en unzip


#Volgende stap is het selecteren van enkele meow polygonen/provincies/realms en de overlapping te plotten met gazetteer gebieden
#https://cran.r-project.org/web/packages/sp/vignettes/over.pdf
#https://www.rdocumentation.org/packages/rgeos/versions/0.4-2/topics/gUnion

#vergelijken met uitkomst in aquamaps

download_db(force = TRUE)
my_db <- aquamapsdata:::src_sqlite_aquamapsdata()
#SQL QUERY => get data for abra alba 
my_db %>% tbl("nativemaps")
my_db %>% tbl("hcaf")
my_db %>% tbl("hspen")
my_db %>% tbl("occ")
my_db %>% tbl("taxa")
