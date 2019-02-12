help(package = "mregions")

rnames <- mr_names("MarineRegions:iho")
rnames2<-mr_names("MarineRegions:longhurst")
mr_names_search(rnames, "IHO")
mr_names_search(rnames, "EEZ")
mr_names_search(rnames, "MEOW")

res1 <- mr_records_by_type(type = "EEZ") # records by type

# if (requireNamespace("geojsonio")) {
#  library("geojsonio")
#   as.json(unclass(res1)) %>% map_leaf
#   as.json(unclass(mr_geojson("Ecoregions:ecoregions"))) %>% map_leaf()
mr_names_search(rnames, "LME")
mr_names_search(rnames, "Longhurst")
mr_names()
placetypes <- mr_place_types()
head(placetypes)
View(placetypes)# check longhurst provinces

res<- mr_geojson(key = "MarineRegions:longhurst_longhurst") #NECS? hoe specifieke polygonen selecteren? id's bij region names?
res<- mr_geojson(key= "MarineRegions:eez") #EEZ
res<- mr_geojson(key= "Ecoregions:ecoregions") #MEOW
res<-mr_geojson(key="MarineRegions:lme") #LME
res
mr_as_wkt(res, fmt = 5) # GeoJSON to WKT
#install.packages("geojsonio")
#if (requireNamespace("geojsonio")) {
#library("geojsonio")
#as.json(unclass(res)) %>% map_leaf}
data <- occurrence("Abra alba", geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")
#Get occurrences by geometry
leaflet() %>%
  addProviderTiles(provider = 'OpenStreetMap') %>%
  addGeoJSON(geojson = res$features) 
leaflet(data)
