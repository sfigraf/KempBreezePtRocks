library(tidyverse)
library(janitor)

# Field and Survey Data joining -------------------------------------------

#fieldData
tagInfo <- read_csv("allAfterFieldData.csv")
  # mutate(RecapID_Clean = as.numeric(RecapID_Clean)#, 
  #        #DeployID = as.numeric(DeployID)
  #        )
surveyInfo <- read_csv("allKBAfterSurveyPoints.csv") #%>%
  #mutate(Point = as.character(Point))

###since survey point numbers can be deployIDs or recapIDs, need to join with both recapID and deployID
#surveyID dtermines if we get the right location
#this is just the recapped instances
recapsOnly <- inner_join(tagInfo, surveyInfo, by = c("SurveyID", "RecapID_Clean" = "Point"))
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
  left_join(surveyInfo, by = c("SurveyID", "DeployID" = "Point"))

###combine these 2 datasets to get location info for deploys and recaps
# shold be same amount of rows as orignal TagInfo
allSurveyandField <- rbind(deploysSurveyInfoJoined, recapsOnly)



# Attribute Info Joining --------------------------------------------------
attributeInfo <- read_csv("attributeInfo.csv")

surveyFieldAttribute <- allSurveyandField %>%
  left_join(attributeInfo, by = c("TagID" = "TagID_Corrected"))



# QAQC --------------------------------------------------------------------

library(leaflet)
library(sf)

arkStreamNetwork1 <- st_zm(arkStreamNetwork, drop = TRUE, what = "ZM")

#change data to sf object in preparation for spatial join with same crs as streamNetwork
surveySitesSF <- st_as_sf(data, coords = c("UTMX", "UTMY"), crs = st_crs(arkStreamNetwork), remove = FALSE)

surveyFieldAttributeSF <- surveyFieldAttribute