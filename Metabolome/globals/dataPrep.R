sourceData <- readRDS('Data/sourceData.rds')

recordIDs <- unique(sourceData$RecordID)

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")

analytes <- sourceData %>%
  select(Analyte) %>%
  unique() %>%
  pull()


######################## 

# library(dplyr)
# sourceData <- read.delim(file.choose(),stringsAsFactors=FALSE)
# ParticipantEncounter <- read.delim(file.choose(),stringsAsFactors=FALSE)
# 
# nrow(sourceData)
# nrow(ParticipantEncounter)
# 
# glimpse(sourceData)
# 
# sourceData$Sex <- NULL
# sourceData$Status <- NULL
# sourceData$Visit <- NULL
# 
# ## should return 0 row tibble
# sourceData %>%
#   group_by(Analyte,RecordID) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
# 
# Participant <- ParticipantEncounter %>%
#   select(RecordID,Sex,Age,Status) %>%
#   mutate(AgeGroup = case_when(Age == "Under 21" ~ "Under 21", Age != "Under 21" ~ "21 & Over")) %>%
#   select(RecordID,Sex,AgeGroup,Status) %>%
#   unique()
# 
# sourceData <- sourceData %>%
#   inner_join(Participant,by="RecordID") %>%
#   mutate(MeasuredValue = adjusted_relative_abundance, Measurement = 'adjusted relative abundance') %>%
#   select(-c(adjusted_relative_abundance))
# 
# rm(Participant)
# rm(ParticipantEncounter)
# 
# saveRDS(sourceData,'Data/sourceData.rds')
