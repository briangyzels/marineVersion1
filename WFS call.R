# Get a JSON response
GAZT_json <- GET("http://marineregions.org/rest/getGazetteerWMSes.json/21460/")
stop_for_status(GAZT_json)  # convert HTTP errors to R errors
names(GAZT_json)
GAZT_json$status_code
GAZT_json$headers$`content-type`


# Parse with httr
json_text_content <- content(GAZT_json, as = "text")
json_text_content
json_parsed_content <- content(GAZT_json, as = "parsed")
json_parsed_content
names(json_parsed_content)

# Get a 2nd JSON response
#<<<<<<< HEAD
#GAZT2_json <- GET( "http://geo.vliz.be/geoserver/wfs?request=getfeature"&service=wfs, version=1.1.0, typename=MarineRegions:longhurst, outputformat=json, filter=<PropertyIsEqualTo><PropertyName>provcode</PropertyName><Literal>NECS</Literal></PropertyIsEqualTo>)
# stop_for_status(GAZT2_json)  # convert HTTP errors to R errors
# names(GAZT2_json)
# GAZT2_json$status_code
# GAZT2_json$headers$`content-type`
#=======
GAZT2_json <- GET( "http://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:longhurst&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eprovcode%3C/PropertyName%3E%3CLiteral%3ENECS%3C/Literal%3E%3C/PropertyIsEqualTo%3E")
stop_for_status(GAZT2_json)  # convert HTTP errors to R errors
names(GAZT2_json)
GAZT2_json$status_code
GAZT2_json$headers$`content-type`
mapview(GAZT2_json, label=GAZT2_json$status_code, color = "darkgreen", col.regions = "green", alpha.regions = .05)

#>>>>>>> 17c16d8c775c822f27eb48d5ab71a72fdd4af034

#Using SF and MAPVIEW package
#<<<<<<< HEAD
baseurl <- "http://geo.vliz.be/geoserver/wfs?"
#&propertyName=provcode&literal=NECS
wfs_request <- "request=GetFeature&service=WFS&version=1.1.0&typeName=MarineRegions:longhurst&outputFormat=json"
fi_regions_wfs <- paste0(baseurl,wfs_request)
fi_regions <- st_read(fi_regions_wfs)
fi_regions <- subset(fi_regions,propertyname ="provcode" )
head(fi_regions)

mapview(fi_regions$geometry, label = fi_regions$provcode, color = "darkgreen", col.regions = "green", alpha.regions = .05)
#=======
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
             CQL_FILTER = spiritf(provcode 'LIKE NECS', provdescr 'LIKE Coastal - NE Atlantic Shelves Province' ,x = 5.12586490079 ,
                                  y = 55.8121098403 , area_m2=2647804822160 , orig_fid = 21 , mrgid=21460))

result_wfs <- GET(wfs_request, query = query)
result_wfs
result <- st_read(result_wfs)
head(result)


#<<<<<<< HEAD
mapview(result, label = result$provcode, color = "darkgreen", col.regions = "green", alpha.regions = .05)
#>>>>>>> c1ad25807151d0078af3887d148056f98df6db5a
# =======
mapview(result, label = result$NECS, color = "darkgreen", col.regions = "green", alpha.regions = .05)
#>>>>>>> 17c16d8c775c822f27eb48d5ab71a72fdd4af034

# Convert parsed JSON file to data frame
json_fi_regions_df<- as.data.frame(fi_regions)
View(json_fi_regions_df)
head(json_fi_regions_df)
#https://www.rstudio.com/resources/videos/using-web-apis-from-r/
