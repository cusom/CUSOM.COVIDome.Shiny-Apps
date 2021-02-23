######################## REFRESH SOURCE DATA #############################

library(dplyr)
sourceData <- read.delim(file.choose(),stringsAsFactors=FALSE)
serogroups <- read.delim(file.choose(), stringsAsFactors = FALSE)
metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)

nrow(sourceData)
nrow(serogroups)
nrow(metadata)

glimpse(sourceData)

sourceData$Platform <- "MSD"
colnames(sourceData)[which(grepl('AnalyteID',colnames(sourceData)))] <- "AnalyteID"
colnames(sourceData)[which(grepl('Display_',colnames(sourceData)))] <- "Analyte"

sourceData %>%
  group_by(Platform) %>%
  summarise(n = n())

## should return 0 row tibble
sourceData %>%
  group_by(Platform,Analyte,RecordID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

Participant <- metadata %>%
  select(RecordID,Sex,Age,Status) %>%
  mutate(AgeGroup = case_when(Age < 21 ~ "Under 21", Age >= 21  ~ "21 & Over")) %>%
  select(RecordID,Sex,Age,AgeGroup,Status) %>%
  unique()

nrow(Participant)

Participant <- Participant %>%
  left_join(serogroups,by="RecordID") %>%
  rename("SeroconversionGroup" = group) %>%
  mutate(SeroconversionGroup = ifelse(SeroconversionGroup=="Neg.",NA,SeroconversionGroup))

nrow(Participant)

sourceData <- sourceData %>%
  inner_join(Participant,by="RecordID") %>%
  mutate(MeasuredValue = calc_conc_imp_mean, Measurement = 'mean normalized concentration') %>%
  select(-c(calc_conc_mean,calc_conc_imp_mean))

rm(Participant)
rm(metadata)
rm(serogroups)

saveRDS(sourceData,'Data/sourceData.rds')