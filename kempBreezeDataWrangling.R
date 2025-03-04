library(tidyverse)
library(janitor)

# Field and Survey Data joining -------------------------------------------

#fieldData
tagInfo <- read_csv("allAfterFieldData.csv")
  # mutate(RecapID_Clean = as.numeric(RecapID_Clean)#, 
  #        #DeployID = as.numeric(DeployID)
  #        )
#get correct locations onto gravel aug and overflow# just needed to do once
# GravelAugandOverflowJoining <- read_csv("GravelAugandOverflowJoining.csv")
# 
# tagInfo1 <- tagInfo %>%
#   left_join(GravelAugandOverflowJoining, by = "TagID")
# 
# write.csv(tagInfo1, "allAfterFieldData1.csv", row.names = F)
# x <- GravelAugandOverflowJoining %>%
#   count(TagID)

##Trimble srvey data from gis
surveyInfo <- read_csv("allKBAfterSurveyPoints.csv") #%>%
  #mutate(Point = as.character(Point))

###since survey point numbers can be deployIDs or recapIDs, need to join with both recapID and deployID
#surveyID dtermines if we get the right location
#this is just the recapped instances
#keep the Point column
recapsOnly <- inner_join(tagInfo, surveyInfo, by = c("SurveyID", "RecapID_Clean" = "Point"), keep = TRUE) 
#this is a helpful qaqc check: the rows that aren't able to get joined bc there is no recapID coorespoinding to Point
#for deploys this is expected since recapID will be NA
# but if a row has a recapID and didn't get joined, that's a problem
deploysAndNA <- anti_join(tagInfo, surveyInfo, by = c("SurveyID", "RecapID_Clean" = "Point"))

#now we take out the recap instances with the deploy ID so we can just get isntances of deployment
deploysOnly <- tagInfo %>%
  #using this filter selects all entries for just deployment surveys
  #should be the same amount of rows as deploysAndNA
  filter(is.na(RecapID_Clean)) 

deploysSurveyInfoJoined <-  deploysOnly %>%
  #this gets rid of all character entries in DeployID so important to make sure everything is a number in this field
  #done for joining
  mutate(DeployID = as.numeric(DeployID)) %>%
  #the filter (is.na(deployID)) helps to see which rows are getting removed from making deployID numeric
  #filter(is.na(DeployID))
  #filter(grepl("Deploy", SurveyID)) %>%
  left_join(surveyInfo, by = c("SurveyID", "DeployID" = "Point"), keep = TRUE)

###combine these 2 datasets to get location info for deploys and recaps
# shold be same amount of rows as orignal TagInfo
allSurveyandField <- rbind(deploysSurveyInfoJoined, recapsOnly)
#if a point doesn't have a northing and easting, it shouldn't have a point either
#because it means it either wasn't found in a relocate survey, or wasn't deployed
#if done right, Point number is same as deployID if there is no RecapID, 
#or same as recapID, and each point id is different
#QAQC
#number of rows with a point should be equal to amount of unique entries, excluding NA entry
nrow(
  allSurveyandField %>%
    filter(!is.na(N))
)
length(unique(allSurveyandField$Point))
#see which points are in there multiple times
# serves to catch potential wonky scenarios and data entry mistakes 
uniquePoints <- allSurveyandField %>%
  count(Point)

# Attribute Info Joining --------------------------------------------------
#comes from 2024_new_KB_tagged_rocks.xlsx in u drive
attributeInfo <- read_csv("attributeInfo.csv")

surveyFieldAttribute <- allSurveyandField %>%
  left_join(attributeInfo, by = c("TagID" = "TagID_Corrected"))

#don't need NA Point entries (means it either wasn't found in a relocate survey, or wasn't deployed)
is.na(surveyFieldAttribute$Notes) <- surveyFieldAttribute$Notes == ""
surveyFieldAttribute1 <- surveyFieldAttribute %>%
  filter(!is.na(Point)) %>%
  mutate(Field_Movement = ifelse(str_detect(RecapID, "\\*"), 1, 0),
         Hiding = ifelse(str_detect(RecapID, "h"), 1, 0),
         Embedded = ifelse(str_detect(RecapID, "e"), 1, 0),
         Buried = ifelse(str_detect(RecapID, "b"), 1, 0), 
         Notes = ifelse(is.na(Notes), "", Notes),
         Comments = ifelse(is.na(Comments), "", Comments),
         Comments = ifelse(grepl("~", RecapID), paste(Comments, "Approximate recap location."), Comments),
         allNotes = case_when(Notes != "" ~ paste(Notes, Comments, sep = ". "), 
                              Notes == "" & Comments != "" ~ Comments, 
                              TRUE ~ ""
         ), 
         Period = "After"
         ) %>%
  rename(Site = Site.x, 
         SurveyID = SurveyID.x, 
         TagSize_mm = `Tag Size (mm)`, 
         A_Axis_mm = `A-axis (mm) Longest`,
         B_Axis_mm = `B-axis (mm) Intermediate`, 
         C_Axis_mm = `C-axis (mm) Thickness`, 
         Gravelometer_mm = `Gravelometer (mm)`, 
         Weight_g = `Weight (g)`, 
         Particle_Class = `Particle Class`,
         Size_Class = `Size Class1`)
#columns from master sheet to make it easier to transfer over in excel
masterSheetColumns <- c("Point",	"E",	"N",	"Elevation",	"Code",	"SurveyID", "Date",	"Period",	"TagID",	"RiffleID",	"TagSize_mm",	"A_Axis_mm",
"B_Axis_mm",	"C_Axis_mm",	"Gravelometer_mm",	"Weight_g",	"Particle_Class",	"Size_Class", "Field_Movement",	"Hiding",	"Embedded",	"Buried",	"allNotes")

surveyFieldAttribute2 <- surveyFieldAttribute1 %>%
  arrange(Point) %>%
  select(all_of(masterSheetColumns))
#this is the final saved datasheet copied into master excel file in U drive
write.csv(surveyFieldAttribute2, "surveyFieldAttribute.csv", row.names = F)


# QAQC --------------------------------------------------------------------

library(leaflet)
library(sf)
surveyFieldAttributeSF <- surveyFieldAttribute %>%
  filter(!is.na(N))
#arkStreamNetwork1 <- st_zm(arkStreamNetwork, drop = TRUE, what = "ZM")
#From GIS:
# NAD_1983_StatePlane_Colorado_North_FIPS_0501_Feet
# WKID: 2231 Authority: EPSG

#change data to sf object in preparation for spatial join with same crs as streamNetwork
surveyFieldAttributeSF1 <- st_as_sf(surveyFieldAttributeSF, coords = c("E", "N"), crs = st_crs("EPSG:2231"), remove = FALSE)

latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326
#transform to new crs for plotting with leaflet
surveyFieldAttributeSF2 <- st_transform(surveyFieldAttributeSF1, latLongCRS) 


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
  addAwesomeMarkers(data = depl2023,
                    group = "Deploy 2023",
                    icon = leaflet::awesomeIcons(
                      icon = 'add',
                      library = 'ion',
                      #iconHeight = 20,
                      markerColor = "purple"
                    ), 
                    popup = paste(
                      "Deploy 2023", "<br>", 
                      "Deploy ID: ", depl2023$DeployID, "<br>", 
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
                      markerColor = "green"
                    ), 
                    popup = paste(
                      "Relocate 2023", "<br>", 
                      "Relocate ID: ", rel2023$RecapID_Clean, "<br>", 
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
                      markerColor = "red"
                    ), 
                    popup = paste(
                      "Deploy 2024_04", "<br>", 
                      "Deploy ID: ", depl2024_04$DeployID, "<br>", 
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
                      markerColor = "blue"
                    ), 
                    popup = paste(
                      "Relocate 2024", "<br>", 
                      "Relocate ID: ", rel2024$RecapID_Clean, "<br>", 
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
                      markerColor = "orange"
                    ), 
                    popup = paste(
                      "Deploy 2024_10", "<br>", 
                      "Deploy ID: ", depl2024_10$DeployID, "<br>", 
                      "Tag ID: ", depl2024_10$TagID, "<br>",
                      "N:", depl2024_10$N, "<br>",
                      "E:", depl2024_10$E, "<br>"
                    )
                    #clusterOptions = markerClusterOptions()
  ) %>%
  addLayersControl(overlayGroups = c("Deploy 2023", "Relocate 2023", "Deploy 2024_04", "Relocate 2024", "Deploy 2024_10"), 
                   baseGroups = c("OSM", "Satellite")) %>%
  addMeasure(primaryLengthUnit = "feet")



# Movement Calculations ---------------------------------------------------

#this is the combined file from KB_Survey_PITRocks_Master_20250213  on U drive
#this is basically the encounter history
AllPitRockData <- read_csv("AllPitRockData.csv")
#cumulative distance by period
AllPitRockData1 <- AllPitRockData %>%
  mutate(Date = mdy(Date)) %>%
  #grepl rather than == gets pitrck? entries. maybe should delete question mark in data
  filter(grepl("PITRCK", Code))

#get distance between found/deploy and next found
allDistance <- AllPitRockData1 %>%
  group_by(TagID, Period) %>%
  arrange(Date) %>%
  #this projection is in feet so it doesn't need a conversion
  mutate(Distance = round(sqrt((N - lag(N))^2 + (E - lag(E))^2), 2)
         #TimePeriodDuration = paste(lag(Date), Date, sep = " - ")
         #D_ft = round(Distance * 3.28084, 2)
         ) #%>%
#this df should be empty
# x <- allDistance %>%
#   filter(Distance > 0,
#          grepl("Deploy", SurveyID))

#total it all up
summaryFile <- allDistance %>%
  group_by(TagID, Period) %>%
  arrange(Date) %>%
  summarize(
    #keep atribute cols
    deployRiffleID = unique(RiffleID),
    TagSize_mm = unique(TagSize_mm),
    A_Axis_mm = unique(A_Axis_mm),
    B_Axis_mm = unique(B_Axis_mm),
    C_Axis_mm = unique(C_Axis_mm),
    Gravelometer_mm = unique(Gravelometer_mm),
    Weight_g = unique(Weight_g),
    Particle_Class = unique(Particle_Class),
    Size_Class = unique(Size_Class),
    totalDistance_ft = sum(Distance, na.rm = T), 
            movementDuration = paste(first(Date), last(Date), sep = " to "),
            #first date should always be deploy date
            DeployDate = first(Date), 
            #plyr ifelse preserves Date type
            LastRecapDate = dplyr::if_else(last(Date) == first(Date), NA, last(Date)), 
             
            #takes from column Point where min date is 
            deployID = Point[which.min(Date)]
  )

write.csv(summaryFile, "summaryFile.csv", row.names = FALSE)




#######need to get distance moved by runoff year
###2019
mov2019 <- AllPitRockData1 %>%
  filter(SurveyID %in% c("Relocate 2019", "Deploy 2019")) %>%
  mutate(Year = 2019) %>%
  group_by(TagID) %>%
  arrange(Date) %>%
  mutate(Distance = round(sqrt((N - lag(N))^2 + (E - lag(E))^2), 2)
  ) %>% 
  filter(SurveyID == "Relocate 2019")

###2020
mov2020 <- AllPitRockData1 %>%
  filter(SurveyID %in% c("Relocate 2019", "Relocate 2020")) %>%
  mutate(Year = 2020) %>%
  group_by(TagID) %>%
  arrange(Date) %>%
  mutate(Distance = round(sqrt((N - lag(N))^2 + (E - lag(E))^2), 2)
  ) %>% 
  filter(SurveyID == "Relocate 2020")

###2021
mov2021 <- AllPitRockData1 %>%
  filter(SurveyID %in% c("Relocate 2022", "Relocate 2020")) %>%
  mutate(Year = 2021) %>%
  group_by(TagID) %>%
  arrange(Date) %>%
  mutate(Distance = round(sqrt((N - lag(N))^2 + (E - lag(E))^2), 2)
  ) %>% 
  filter(SurveyID == "Relocate 2022")

###2023
mov2023 <- AllPitRockData1 %>%
  filter(SurveyID %in% c("Relocate 2023", "Deploy 2023")) %>%
  mutate(Year = 2023) %>%
  group_by(TagID) %>%
  arrange(Date) %>%
  mutate(Distance = round(sqrt((N - lag(N))^2 + (E - lag(E))^2), 2)
  ) %>% 
  filter(SurveyID == "Relocate 2023")

#for known data already, this df should be the same number of rows as data already in master file
mov2023QAQC <- mov2023 %>%
  filter(!is.na(Distance))
# 
# test <- x2023Distance %>%
#   filter(SurveyID %in% c("Relocate 2023"))
#this is a list of tags from 2023 movement list master that had movements according to eric. 
#good for QAQC to see potentially which tags didn't get entered in the datasheet
# masterFile2023Tags <- read_csv("masterFile2023Tags.csv")
# potentialTagsNotEnteredCorrectly <- masterFile2023Tags %>%
#   anti_join(mov2023QAQC, by = c("tagsInaMasterfile" = "TagID"))

###2024
mov2024 <- AllPitRockData1 %>%
  filter(SurveyID %in% c("Relocate 2024", "Deploy 2024_04", "Relocate 2023")) %>%
  mutate(Year = 2024) %>%
  group_by(TagID) %>%
  arrange(Date) %>%
  mutate(Distance = round(sqrt((N - lag(N))^2 + (E - lag(E))^2), 2)
  ) %>%
  filter(SurveyID == "Relocate 2024")

###bding all together
allMovements = list(
  mov2019, 
  mov2020, 
  mov2021, 
  mov2023, 
  mov2024
)
allMovementdataCombined <- dplyr::bind_rows(allMovements)

allMovementdataCombined1 <- allMovementdataCombined %>%
  ungroup() %>%
  rename(Distance_ft = Distance) %>%
  mutate(Size_Class2 = gsub('[[:digit:]]+', '', Size_Class), 
         Distance_m = Distance_ft *0.3048, 
         B_Axis_ft = B_Axis_mm * 0.00328084, 
         Moved_1PD = ifelse(Distance_ft > B_Axis_ft, 1, 0), 
         Moved_2PD = ifelse(Distance_ft > 2*B_Axis_ft, 1, 0)
         )

#adding old notes
#comes from old master File
oldMasterMovementsCombined <- read_csv("oldMasterMovementsCombined.csv")

##adding notes 
allMovementdataCombined2 <- allMovementdataCombined1 %>%
  left_join(oldMasterMovementsCombined[,c("Point", "TagID", "SurveyID", "Field_Movement", "Hiding", "Embedded", "Buried", "Notes")], by = c("Point", "TagID", "SurveyID")) %>%
  mutate(Field_Movement = coalesce(Field_Movement.x, Field_Movement.y),
        Hiding = coalesce(Hiding.x, Hiding.y), 
         Buried = coalesce(Buried.x, Buried.y), 
         Embedded = coalesce(Embedded.x, Embedded.y)
         ) %>% 
  #unite is ogod for pasting columns together
  unite("Notes", c("Notes.x", "Notes.y"), sep = ". ",  na.rm = TRUE) %>%
  #unite("Field_Movement", c("Field_Movement.x", "Field_Movement.y"), sep = ". ",  na.rm = TRUE, remove = FALSE) %>%
  mutate(Site = case_when(RiffleID == 1 ~ "Riffle 1", 
                          RiffleID %in% c("2A", "2B", "2") ~ "Riffle 2", 
                          RiffleID == 3 ~ "Riffle 3", 
                          grepl("GA", RiffleID) ~ "GravelAug",
                          grepl("Overflow", RiffleID) ~ "Overflow"
                          ))
# cat(names(oldMasterMovementsCombined), sep = "', '")
# names(oldMasterMovementsCombined)
columnNames <- c('Point', 'E', 'N', 'Elevation', 'Code', 'SurveyID', 'Year', 'Period', 'TagID', 'RiffleID', 'Site', 'TagSize_mm', 
                 'A_Axis_mm', 'B_Axis_mm', 'B_Axis_ft', 'C_Axis_mm', 'Gravelometer_mm', 'Weight_g', 'Particle_Class', 'Size_Class', 'Size_Class2', 
                 'Distance_ft', 'Distance_m', 
                 'Field_Movement', 'Moved_1PD', 'Moved_2PD', 'Hiding', 'Embedded', 'Buried', 'Notes')
#NA readings in Movement field are due to movements that don't occur in the correct runoff year; these will show up in cumulative movement
allMovementdataCombined3 <- allMovementdataCombined2 %>%
  select(all_of(columnNames))
###This is what goes in the master file for movementsCombined
write.csv(allMovementdataCombined3, "AllMovementsCombined.csv", row.names = FALSE)
# names(allMovementdataCombined2)

####


##QAQC
x <- allMovementdataCombined3[,c("Point", "TagID", "SurveyID", "Distance_ft")] %>%
  left_join(oldMasterMovementsCombined[,c("Point", "TagID", "SurveyID", "Distance_ft")], by = c("Point", "TagID", "SurveyID")) %>%
  #sees how the difference is between old movements and R calculated ones
  mutate(dif = Distance_ft.x - Distance_ft.y)
  
#seeing how many entries each one has
x <- oldMasterMovementsCombined %>%
  count(SurveyID)

y <- allMovementdataCombined3 %>%
  count(SurveyID)

y1 <- AllPitRockData %>%
  count(SurveyID)

xx <- AllPitRockData %>%
  filter(SurveyID == "Relocate 2019") %>%
  anti_join(allMovementdataCombined3 %>%
              filter(SurveyID == "Relocate 2019"), by = "TagID" )

# masterFile2023Tags <- read_csv("masterFile2023Tags.csv")
# 
# 
# test <- allMovementdataCombined1 %>%
#   filter(Moved_2PD == 1)
# 
# potentialTagsNotEnteredCorrectly <- masterFile2023Tags %>%
#   anti_join(test, by = c("TagID"))


# x2023 <- AllPitRockData1 %>%
#   filter(Year == 2023)
# #get pairs of deploy/relocates
# deploys <- x2023 %>%
#   filter(grepl("Deploy", SurveyID)) 
# relocates <- x2023 %>%
#   filter(grepl("Relocate", SurveyID)) %>%
#   select(TagID, Year, N, E) %>%
#   rename(relocate_N = N, 
#          relocate_E = E)
# 
# #left join gets all rocks and gives NA if they weren't found that year, inner join would get only deploy/relocate parings
# deploysRelocatesPaired <- deploys %>%
#   #group_by(Period) %>%
#   left_join(relocates, by = c("TagID", "Year")) #228000607501
#   
# #getting ditsance for that year
# x2023Distance <- deploysRelocatesPaired %>%
#   mutate(Distance = round(sqrt((relocate_N - N)^2 + (relocate_E - E)^2), 2)
#   )

