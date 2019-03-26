library("mregions")
library("worrms")
ABRALBADATA <- wm_distribution_(141433)
install.packages("igraph")
library("igraph")
library("devtools")

types <- mr_place_types()
reg<- mr_records_by_type(type = "EEZ")
reg2<- mr_records_by_type(type = "EEZ", offset=100)
reg3<- mr_records_by_type(type = "EEZ", offset=200)
meow<- mr_records_by_type(type = "Marine Ecoregion of the World (MEOW)")
meow2<- mr_records_by_type(type = "Marine Ecoregion of the World (MEOW)", offset=100)
meow3<- mr_records_by_type(type = "Marine Ecoregion of the World (MEOW)", offset=200)

longhurst<- mr_records_by_type(type = "Longhurst Province")
town<- mr_records_by_type(type = "Town") 
town2<- mr_records_by_type(type = "Town", offset=100)
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
shp <- mr_shp(key= "MarineRegions:eez",filter = "Belgian Exclusive Economic Zone")
wkt <- mr_as_wkt(shp)
library('httr')
library('data.table')
args <- list(scientificname = "Abra alba", geometry = wkt, limit = 100)
res <- httr::GET('http://api.iobis.org/occurrence', query = args)
xx <- data.table::setDF(data.table::rbindlist(httr::content(res)$results, use.names = TRUE, fill = TRUE))
xx <- xx[, c('scientificName', 'decimalLongitude', 'decimalLatitude')]
names(xx)[2:3] <- c('longitude', 'latitude')
library('leaflet')
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = xx) %>%
  addPolygons(data = shp)

#MEOW, LME, ... polygonen selecteren die overlappen met gebied uit gazetteer
shp <- mr_geojson(name= "meow") 
shp <- mr_shp(key= "MarineRegions:meow")#dit argument lijkt niet te bestaan?
#http://www.imachordata.com/meow-its-marine-ecoregions-in-r-2/
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
#https://databasin.org/datasets/3b6b12e7bcca419990c9081c0af254a2 download shapefiles in zip bestand en unzip
ogrInfo("../../MEOW-TNC", "meow_ecos") # naam mgl veranderen
#Unite the spatial polygons for each region into one
provinces <- unionSpatialPolygons(regions, regions$PROVINCE) #hogere orde= gemakkelijker?
#Make a data frame that will have Province level info and above
prov_data <- regions@data %>%
  group_by(PROVINCE) %>%
  summarise(PROV_CODE = PROV_CODE[1], REALM = REALM[1], RLM_CODE=RLM_CODE[1], Lat_Zone=Lat_Zone[1])
#merge the polygons with the new data file
#note the row.names argument to make sure they map to each other
provinces <- SpatialPolygonsDataFrame(provinces, 
                                      data=data.frame(
                                        join(data.frame(PROVINCE=names(provinces)),
                                             prov_data),
                                        row.names=row.names(provinces)))
#make spatial polygons for realms
realms <- unionSpatialPolygons(regions, regions$REALM)

#make new data
realm_data <- regions@data %>%
  group_by(REALM) %>%
  summarise(RLM_CODE = RLM_CODE[1],  Lat_Zone=Lat_Zone[1])

#merge the two!
realms <- SpatialPolygonsDataFrame(realms, 
                                   data=data.frame(
                                     join(data.frame(REALM=names(realms)),
                                          realm_data),
                                     row.names=row.names(realms)))
#########Plot them all
par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(regions, main="Ecoregion", cex.main=5)
plot(provinces, lwd=2, border="red", main="Province")
plot(realms, lwd=2, border="blue", main="Realm")
par(mfrow=c(1,1))
## nicer figures => GGPLOT2

#Volgende stap is het selecteren van enkele meow polygonen/provincies/realms en de overlapping te plotten met gazetteer gebieden
#https://cran.r-project.org/web/packages/sp/vignettes/over.pdf
#https://www.rdocumentation.org/packages/rgeos/versions/0.4-2/topics/gUnion

#vergelijken met uitkomst in aquamaps
install_github("raquamaps/aquamapsdata")
library("aquamapsdata")
library("purrr")
download_db(force = TRUE)
my_db <- aquamapsdata:::src_sqlite_aquamapsdata()
my_db %>% tbl("nativemaps")
my_db %>% tbl("hcaf")
my_db %>% tbl("hspen")
my_db %>% tbl("occ")
my_db %>% tbl("taxa")
