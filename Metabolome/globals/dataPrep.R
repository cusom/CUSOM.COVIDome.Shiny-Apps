sourceData <- readRDS('Data/sourceData.rds')

recordIDs <- unique(sourceData$RecordID)

platforms <- sourceData %>%
  select(Platform) %>%
  unique() %>%
  pull()

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")

PlasmaMetabolites <- sourceData %>%
  filter(Platform=="Plasma") %>%
  select(Analyte) %>%
  unique() %>%
  pull()

RedBloodCellMetabolites <- sourceData %>%
  filter(Platform=="Red Blood Cells") %>%
  select(Analyte) %>%
  unique() %>%
  pull()

######################## 

# library(dplyr)
# plasma <- read.delim(file.choose(),stringsAsFactors=FALSE)
# rbc <- read.delim(file.choose(),stringsAsFactors=FALSE)
# metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)
# 
# nrow(plasma)
# nrow(rbc)
# nrow(metadata)
# 
# glimpse(plasma)
# glimpse(rbc)
# 
# unique(rbc$Specimen_type)
# 
# rbc$Specimen_type <- "Red Blood Cells"
# 
# setdiff(colnames(plasma),colnames(rbc))
# 
# plasma$Sex <- NULL
# plasma$Status <- NULL
# plasma$Visit <- NULL
# 
# setdiff(colnames(plasma),colnames(rbc))
# 
# sourceData <- rbind(plasma,rbc)
# colnames(sourceData)[which(colnames(sourceData)=="Specimen_type")] <- "Platform"
# 
# nrow(sourceData)
# nrow(plasma)+nrow(rbc)
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
# sourceData <- sourceData %>%
#   inner_join(Participant,by="RecordID") %>%
#   mutate(MeasuredValue = adjusted_relative_abundance, Measurement = 'adjusted relative abundance') %>%
#   select(-c(adjusted_relative_abundance))
# 
# rm(plasma)
# rm(rbc)
# rm(Participant)
# rm(metadata)
# 
# saveRDS(sourceData,'Data/sourceData.rds')
