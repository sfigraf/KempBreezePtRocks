####html map/QAQC

AllPitRockData <- read_csv("AllPitRockData.csv") %>%
  filter(!is.na(N))

library(leaflet)
library(sf)
#once the map is in the viewer, you can save it if you want as an itneractive html by selecting "export" -> "save as web page"

# x <- allMovementdataCombined1 %>%
#   filter(!is.na(N))
#From GIS:
# NAD_1983_StatePlane_Colorado_North_FIPS_0501_Feet
# WKID: 2231 Authority: EPSG

#change data to sf object in preparation for spatial join with same crs
surveyFieldAttributeSF1 <- st_as_sf(AllPitRockData, coords = c("E", "N"), crs = st_crs("EPSG:2231"), remove = FALSE)

latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326
#transform to new crs for plotting with leaflet
surveyFieldAttributeSF2 <- st_transform(surveyFieldAttributeSF1, latLongCRS) 

depl2019 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Deploy 2019")
rel2019 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Relocate 2019")
rel2020 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Relocate 2020")
rel2022 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Relocate 2022")

depl2024_10 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Deploy 2024_10")
depl2024_04 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Deploy 2024_04")
depl2023 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Deploy 2023")
rel2023 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Relocate 2023")
rel2024 <- surveyFieldAttributeSF2 %>%
  filter(SurveyID == "Relocate 2024")


leaflet() %>%
  addTiles(options = providerTileOptions(maxZoom = 100), group = "OSM") %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 100), 
                   group = "Satellite"
  ) %>%
  addAwesomeMarkers(data = depl2019,
                    group = "Deploy 2019",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "red"
                    ), 
                    popup = paste(
                      "Deploy 2019", "<br>", 
                      "Deploy ID: ", depl2019$Point, "<br>", 
                      "Tag ID: ", depl2019$TagID, "<br>",
                      "N:", depl2019$N, "<br>",
                      "E:", depl2019$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = rel2019,
                    group = "Relocate 2019",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "darkred"
                    ), 
                    popup = paste(
                      "Relocate 2019", "<br>", 
                      "Relocate ID: ", rel2019$Point, "<br>", 
                      "Tag ID: ", rel2019$TagID, "<br>",
                      "N:", rel2019$N, "<br>",
                      "E:", rel2019$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = rel2020,
                    group = "Relocate 2020",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "cadetblue"
                    ), 
                    popup = paste(
                      "Relocate 2020", "<br>", 
                      "Relocate ID: ", rel2020$Point, "<br>", 
                      "Tag ID: ", rel2020$TagID, "<br>",
                      "N:", rel2020$N, "<br>",
                      "E:", rel2020$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = rel2022,
                    group = "Relocate 2022",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "orange"
                    ), 
                    popup = paste(
                      "Relocate 2022", "<br>", 
                      "Relocate ID: ", rel2022$Point, "<br>", 
                      "Tag ID: ", rel2022$TagID, "<br>",
                      "N:", rel2022$N, "<br>",
                      "E:", rel2022$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  
  addAwesomeMarkers(data = depl2023,
                    group = "Deploy 2023",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "green"
                    ), 
                    popup = paste(
                      "Deploy 2023", "<br>", 
                      "Deploy ID: ", depl2023$Point, "<br>", 
                      "Tag ID: ", depl2023$TagID, "<br>",
                      "N:", depl2023$N, "<br>",
                      "E:", depl2023$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = rel2023,
                    group = "Relocate 2023",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "darkgreen"
                    ), 
                    popup = paste(
                      "Relocate 2023", "<br>", 
                      "Relocate ID: ", rel2023$Point, "<br>", 
                      "Tag ID: ", rel2023$TagID, "<br>",
                      "N:", rel2023$N, "<br>",
                      "E:", rel2023$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = depl2024_04,
                    group = "Deploy 2024_04",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "purple"
                    ), 
                    popup = paste(
                      "Deploy 2024_04", "<br>", 
                      "Deploy ID: ", depl2024_04$Point, "<br>", 
                      "Tag ID: ", depl2024_04$TagID, "<br>",
                      "N:", depl2024_04$N, "<br>",
                      "E:", depl2024_04$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = rel2024,
                    group = "Relocate 2024",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "darkpurple"
                    ), 
                    popup = paste(
                      "Relocate 2024", "<br>", 
                      "Relocate ID: ", rel2024$Point, "<br>", 
                      "Tag ID: ", rel2024$TagID, "<br>",
                      "N:", rel2024$N, "<br>",
                      "E:", rel2024$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addAwesomeMarkers(data = depl2024_10,
                    group = "Deploy 2024_10",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "blue"
                    ), 
                    popup = paste(
                      "Deploy 2024_10", "<br>", 
                      "Deploy ID: ", depl2024_10$Point, "<br>", 
                      "Tag ID: ", depl2024_10$TagID, "<br>",
                      "N:", depl2024_10$N, "<br>",
                      "E:", depl2024_10$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addLayersControl(overlayGroups = c("Deploy 2019", "Relocate 2019", "Relocate 2020", "Relocate 2022",
                                     "Deploy 2023", "Relocate 2023", "Deploy 2024_04", "Relocate 2024", "Deploy 2024_10"), 
                   baseGroups = c("OSM", "Satellite")) %>%
  addMeasure(primaryLengthUnit = "feet")
