sourceData <- readRDS('Data/sourceData.rds')

Platforms <- sort(unique(sourceData$Platform))

recordIDs <- unique(sourceData$RecordID)

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")

######################## 

# library(dplyr)
# sourceData <- read.delim(file.choose(),stringsAsFactors=FALSE)
# metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)
# 
# nrow(sourceData)
# nrow(metadata)
# glimpse(sourceData)
# 
# sourceData <- sourceData %>%
#   mutate("Measurement" = "percent") %>%
#   rename("Platform" = Lineage, "Analyte" = Cell_population, "MeasuredValue" = percent)
# 
# ## should return 0 row tibble
# sourceData %>%
#   group_by(Platform,Analyte,RecordID) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
# 
# Participant <- metadata %>%
#   select(RecordID,Sex,Age,Status) %>%
#   mutate(AgeGroup = case_when(Age == "Under 21" ~ "Under 21", Age != "Under 21" ~ "21 & Over")) %>%
#   select(RecordID,Sex,AgeGroup,Status) %>%
#   unique()
# 
# 
# sourceData <- sourceData %>%
#   inner_join(Participant,by="RecordID")
# 
# rm(Participant)
# rm(metadata)
# 
# saveRDS(sourceData,'Data/sourceData.rds')
