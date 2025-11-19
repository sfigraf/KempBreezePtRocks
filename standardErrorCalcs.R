###mobile/trimble comparison
#standard error calculation
library(tidyverse)
library(writexl)
#test tags for exclusion copied and pasted from "U:\Projects\Colorado_River\Kemp_Breeze_SWA\Data\Sediment\PIT_Tagged_Rocks\Data\Detections\KB_PITRocks_AllDetections_2025_09.xlsx"
testTags <- c(226001546996, 226001581072, 230000004000, 230000087405,
              230000087408, 230000102750, 230000228791, 230000142503)

# need to bring in all trimble field data from 2025
trimble2025 <- read_csv("InputData/AllPitRockData.csv") %>%
  filter(SurveyID == "Relocate 2025", 
         DetectionType == "Trimble", 
         !TagID %in% testTags)
#all m3 from 2025
M3_Detections <- read_csv("InputData/Standard Error Calcs/2025 M3 Detections.csv") %>%
  filter(!TagID %in% testTags)
# all m4 from 2025
M4_Detections <- read_csv("InputData/Standard Error Calcs/2025 M4 Detections.csv") %>%
  filter(!TagID %in% testTags)

#all hpr from 2025
HPR_Detections <- read_csv("InputData/Standard Error Calcs/2025 hpr plus detections.csv") %>%
  filter(!TagID %in% testTags)

#separate to backpack and packraft
M3_Detections1 <- M3_Detections %>%
  mutate(antennaType = case_when(str_detect(Antenna, "Backpack") ~ "Backpack", 
                                 str_detect(Antenna, "Packraft") ~ "Packraft")) %>%
  rename(N = Lat_DD, 
         E = Long_DD) %>%
  select(TagID, E, N, antennaType)

M4_Detections1 <- M4_Detections %>%
  mutate(antennaType = case_when(str_detect(Antenna, "Backpack") ~ "Backpack", 
                                   str_detect(Antenna, "Packraft") ~ "Packraft")) %>%
  rename(N = Lat_DD, 
         E = Long_DD) %>%
  select(TagID, E, N, antennaType)

HPR_Detections1 <- HPR_Detections %>%
  mutate(antennaType = "HPR+") %>%
  rename(N = Latitude, 
         E = Longitude) %>%
  select(TagID, E, N, antennaType)

trimble2025_1 <- trimble2025 %>%
  mutate(antennaTypeTrimble = "Trimble") %>%
  rename(Trimble_N = N, 
         Trimble_E = E) %>%
  select(TagID, Trimble_E, Trimble_N, antennaTypeTrimble)

##starting with standard error against m3.
#Would join them all except there's multiple entries for same tag on same survey. Could probably average these distances though at the end
# gets difference between trimble and M3 tags and excludes tags from trimble without m3 detection

createSummaryTableFunction <- function(trimbleData = trimble2025_1, dataToCompare) {
  #northingCol <- dataToCompare
  trimbleJoined <- trimble2025_1 %>%
    left_join(dataToCompare, by = "TagID") %>%
    mutate(distanceDif_ft = round(sqrt((Trimble_N - N)^2 + (Trimble_E - E)^2), 2),
           northingError = abs(Trimble_N - N), 
           eastingError = abs(Trimble_E - E)) %>%
    filter(!is.na(distanceDif_ft))
  
  trimbleJoinedSummarized <- trimbleJoined %>%
    group_by(antennaType) %>%
    summarise(
      #mean
      meanDistanceDifError = round(mean(distanceDif_ft), 2), 
      meanNorthingError = round(mean(northingError), 2),
      meanEastingError = round(mean(eastingError), 2),
      #min
      minDistanceDifError = round(min(distanceDif_ft), 2), 
      minNorthingError = round(min(northingError), 2),
      minEastingError = round(min(eastingError), 2),
      #max
      maxDistanceDifError = round(max(distanceDif_ft), 2), 
      maxNorthingError = round(max(northingError), 2),
      maxEastingError = round(max(eastingError), 2),
      #median
      medianDistanceDifError = round(median(distanceDif_ft), 2), 
      medianNorthingError = round(median(northingError), 2),
      medianEastingError = round(median(eastingError), 2),
      #standard deviation
      sdDistanceDifErrorTrimblevsM3 = round(sd(distanceDif_ft), 2), 
      sdNorthingError = round(sd(northingError), 2),
      sdEastingError = round(sd(eastingError), 2)
    )
  
  # get data to final display form
  trimbleJoinedSummarizedFinal <- trimbleJoinedSummarized %>%
    pivot_longer(
      cols = -antennaType,
      names_to = c("Statistic", "ErrorType"),
      names_pattern = "(mean|min|max|median|sd)(.*)",
      values_to = "value"
    ) %>%
    mutate(
      Statistic = case_when(str_detect(Statistic, "sd") ~ "SD", 
                            TRUE ~ Statistic),
      ErrorType = case_when(
        str_detect(ErrorType, "Northing") ~ "Northing Error",
        str_detect(ErrorType, "Easting")  ~ "Easting Error",
        str_detect(ErrorType, "Distance") ~ "Absolute Error"
      )
    ) %>%
    pivot_wider(
      names_from = ErrorType,
      values_from = value
    ) %>%
    relocate(`Absolute Error`, .after = last_col()) %>%
    arrange(antennaType, Statistic) #factor(Statistic, levels = c("maximum","mean","median","minimum","SD"))
  return(trimbleJoinedSummarizedFinal)
  
}

allData <- list(trimblevsM3SummarizedFinal = createSummaryTableFunction(dataToCompare =  M3_Detections1), 
                trimblevsM4SummarizedFinal = createSummaryTableFunction(dataToCompare =  M4_Detections1), 
                trimblevsHPRSummarizedFinal = createSummaryTableFunction(dataToCompare =  HPR_Detections1))
write_xlsx(allData, "OutputData/StandardErrorCalculations/errorSummarizedDFs2025.xlsx")  


#getting just backpack error across m3 and m4
trimbleM3backpack <- trimbleM3 %>%
  filter(antennaTypeM3 == "Backpack")

