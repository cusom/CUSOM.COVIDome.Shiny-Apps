sourceData <- readRDS('Data/sourceData.rds')

platforms <- sort(unique(sourceData$Platform))

recordIDs <- unique(sourceData$RecordID)

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")

cytokine <- sourceData %>%
  filter(grepl('MSD',Platform)) %>%
  distinct(Analyte) %>%
  pull(Analyte) %>%
  sort()

cytokines <- as.data.table(cytokine)
rm(cytokine)

######################## 

# library(dplyr)
# sourceData <- read.delim(file.choose(),stringsAsFactors=FALSE)
# metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)
# 
# nrow(sourceData)
# nrow(metadata)
# glimpse(sourceData)
# 
# sourceData$Platform <- "MSD"
# colnames(sourceData)[which(grepl('AnalyteID',colnames(sourceData)))] <- "AnalyteID"
# colnames(sourceData)[which(grepl('Display_',colnames(sourceData)))] <- "Analyte"
# 
# sourceData %>%
#   group_by(Platform) %>%
#   summarise(n = n())
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
# glimpse(sourceData)
# 
# sourceData <- sourceData %>%
#   inner_join(Participant,by="RecordID") %>%
#   mutate(MeasuredValue = calc_conc_imp_mean, Measurement = 'mean normalized concentration') %>%
#   select(-c(calc_conc_mean,calc_conc_imp_mean))
# 
# rm(Participant)
# rm(metadata)
# 
# saveRDS(sourceData,'Data/sourceData.rds')
