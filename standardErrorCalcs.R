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
  mutate(antennaTypeM3 = case_when(str_detect(Antenna, "Backpack") ~ "Backpack", 
                                 str_detect(Antenna, "Packraft") ~ "Packraft")) %>%
  rename(M3_N = Lat_DD, 
         M3_E = Long_DD) %>%
  select(TagID, M3_E, M3_N, antennaTypeM3)

M4_Detections1 <- M4_Detections %>%
  mutate(antennaTypeM4 = case_when(str_detect(Antenna, "Backpack") ~ "Backpack", 
                                   str_detect(Antenna, "Packraft") ~ "Packraft")) %>%
  rename(M4_N = Lat_DD, 
         M4_E = Long_DD) %>%
  select(TagID, M4_E, M4_N, antennaTypeM4)

HPR_Detections1 <- HPR_Detections %>%
  mutate(antennaTypeHPR = "HPR+") %>%
  rename(HPR_N = Latitude, 
         HPR_E = Longitude) %>%
  select(TagID, HPR_E, HPR_N, antennaTypeHPR)

trimble2025_1 <- trimble2025 %>%
  mutate(antennaTypeTrimble = "Trimble") %>%
  rename(Trimble_N = N, 
         Trimble_E = E) %>%
  select(TagID, Trimble_E, Trimble_N, antennaTypeTrimble)

##starting with standard error against m3.
#Would join them all except there's multiple entries for same tag on same survey. Could probably average these distances though at the end
# gets difference between trimble and M3 tags and excludes tags from trimble without m3 detection

########M3
trimbleM3 <- trimble2025_1 %>%
  left_join(M3_Detections1, by = "TagID") %>%
  mutate(distanceDif_ft = round(sqrt((Trimble_N - M3_N)^2 + (Trimble_E - M3_E)^2), 2),
         northingError = abs(Trimble_N - M3_N), 
         eastingError = abs(Trimble_E - M3_E)) %>%
  filter(!is.na(distanceDif_ft))

trimbleM3Summarized <- trimbleM3 %>%
  group_by(antennaTypeM3) %>%
  summarise(
    #mean
    meanDistanceDifErrorTrimblevsM3 = round(mean(distanceDif_ft), 2), 
            meanNorthingError = round(mean(northingError), 2),
            meanEastingError = round(mean(eastingError), 2),
            #min
            minDistanceDifErrorTrimblevsM3 = round(min(distanceDif_ft), 2), 
            minNorthingError = round(min(northingError), 2),
            minEastingError = round(min(eastingError), 2),
    #max
    maxDistanceDifErrorTrimblevsM3 = round(max(distanceDif_ft), 2), 
    maxNorthingError = round(max(northingError), 2),
    maxEastingError = round(max(eastingError), 2),
    #median
    medianDistanceDifErrorTrimblevsM3 = round(median(distanceDif_ft), 2), 
    medianNorthingError = round(median(northingError), 2),
    medianEastingError = round(median(eastingError), 2),
    #standard deviation
    sdDistanceDifErrorTrimblevsM3 = round(sd(distanceDif_ft), 2), 
    sdNorthingError = round(sd(northingError), 2),
    sdEastingError = round(sd(eastingError), 2)
            )

# get data to final display form
trimbleM3SummarizedFinal <- trimbleM3Summarized %>%
  pivot_longer(
    cols = -antennaTypeM3,
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
  #select(-antennaTypeM3) %>%
  pivot_wider(
    names_from = ErrorType,
    values_from = value
  ) %>%
  relocate(`Absolute Error`, .after = last_col()) %>%
  arrange(antennaTypeM3, Statistic) #factor(Statistic, levels = c("maximum","mean","median","minimum","SD"))

############M4
trimbleM4 <- trimble2025_1 %>%
  left_join(M4_Detections1, by = "TagID") %>%
  mutate(distanceDif_ft = round(sqrt((Trimble_N - M4_N)^2 + (Trimble_E - M4_E)^2), 2),
         northingError = abs(Trimble_N - M4_N), 
         eastingError = abs(Trimble_E - M4_E)) %>%
  filter(!is.na(distanceDif_ft))

trimbleM4Summarized <- trimbleM4 %>%
  group_by(antennaTypeM4) %>%
  summarise(
    #mean
    meanDistanceDifErrorTrimblevsM4 = round(mean(distanceDif_ft), 2), 
    meanNorthingError = round(mean(northingError), 2),
    meanEastingError = round(mean(eastingError), 2),
    #min
    minDistanceDifErrorTrimblevsM4 = round(min(distanceDif_ft), 2), 
    minNorthingError = round(min(northingError), 2),
    minEastingError = round(min(eastingError), 2),
    #max
    maxDistanceDifErrorTrimblevsM4 = round(max(distanceDif_ft), 2), 
    maxNorthingError = round(max(northingError), 2),
    maxEastingError = round(max(eastingError), 2),
    #median
    medianDistanceDifErrorTrimblevsM4 = round(median(distanceDif_ft), 2), 
    medianNorthingError = round(median(northingError), 2),
    medianEastingError = round(median(eastingError), 2),
    #standard deviation
    sdDistanceDifErrorTrimblevsM4 = round(sd(distanceDif_ft), 2), 
    sdNorthingError = round(sd(northingError), 2),
    sdEastingError = round(sd(eastingError), 2)
  )


# get table to final form
trimbleM4SummarizedFinal <- trimbleM4Summarized %>%
  pivot_longer(
    cols = -antennaTypeM4,
    names_to = c("Statistic", "ErrorType"),
    names_pattern = "(mean|min|max|median|sd)(.*)",
    values_to = "value"
  ) %>%
  mutate(Statistic = case_when(str_detect(Statistic, "sd") ~ "SD", 
                               TRUE ~ Statistic),
    ErrorType = case_when(
      str_detect(ErrorType, "Northing") ~ "Northing Error",
      str_detect(ErrorType, "Easting")  ~ "Easting Error",
      str_detect(ErrorType, "Distance") ~ "Absolute Error"
    )
  ) %>%
  #select(-antennaTypeM4) %>%
  pivot_wider(
    names_from = ErrorType,
    values_from = value
  ) %>%
  relocate(`Absolute Error`, .after = last_col()) %>%
  arrange(antennaTypeM4, Statistic) #factor(Statistic, levels = c("maximum","mean","median","minimum","SD"))
########### HPR
trimbleHPR <- trimble2025_1 %>%
  left_join(HPR_Detections1, by = "TagID") %>%
  mutate(distanceDif_ft = round(sqrt((Trimble_N - HPR_N)^2 + (Trimble_E - HPR_E)^2), 2),
         northingError = abs(Trimble_N - HPR_N), 
         eastingError = abs(Trimble_E - HPR_E)) %>%
  filter(!is.na(distanceDif_ft))

trimbleHPRSummarized <- trimbleHPR %>%
  group_by(antennaTypeHPR) %>%
  summarise(
    #mean
    meanDistanceDifErrorTrimblevsHPR = round(mean(distanceDif_ft), 2), 
    meanNorthingError = round(mean(northingError), 2),
    meanEastingError = round(mean(eastingError), 2),
    #min
    minDistanceDifErrorTrimblevsHPR = round(min(distanceDif_ft), 2), 
    minNorthingError = round(min(northingError), 2),
    minEastingError = round(min(eastingError), 2),
    #max
    maxDistanceDifErrorTrimblevsHPR = round(max(distanceDif_ft), 2), 
    maxNorthingError = round(max(northingError), 2),
    maxEastingError = round(max(eastingError), 2),
    #median
    medianDistanceDifErrorTrimblevsHPR = round(median(distanceDif_ft), 2), 
    medianNorthingError = round(median(northingError), 2),
    medianEastingError = round(median(eastingError), 2),
    #standard deviation
    sdDistanceDifErrorTrimblevsHPR = round(sd(distanceDif_ft), 2), 
    sdNorthingError = round(sd(northingError), 2),
    sdEastingError = round(sd(eastingError), 2)
  )
##get final table for display
# Pivot the data into long format
trimbleHPRSummarizedFinal <- trimbleHPRSummarized %>%
  pivot_longer(
    cols = -antennaTypeHPR,
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
  arrange(antennaTypeHPR, Statistic) #factor(Statistic, levels = c("maximum","mean","median","minimum","SD"))
########## saviong data

allData <- list(trimbleM3SummarizedFinal =trimbleM3SummarizedFinal, 
                trimbleM4SummarizedFinal = trimbleM4SummarizedFinal, 
                trimbleHPRSummarizedFinal = trimbleHPRSummarizedFinal)
write_xlsx(allData, "errorSummarizedDFs2025.xlsx")  


