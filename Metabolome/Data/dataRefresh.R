######################## REFRESH SOURCE DATA #########################
 
# library(dplyr)
# plasma <- read.delim(file.choose(),stringsAsFactors=FALSE)
# rbc <- read.delim(file.choose(),stringsAsFactors=FALSE)
# serogroups <- read.delim(file.choose(), stringsAsFactors = FALSE)
# metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)
# 
# nrow(plasma)
# nrow(rbc)
# nrow(serogroups)
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
#   mutate(AgeGroup = case_when(Age < 21 ~ "Under 21", Age >= 21  ~ "21 & Over")) %>%
#   select(RecordID,Sex,AgeGroup,Status) %>%
#   unique()
# 
# nrow(Participant)
# 
# Participant <- Participant %>%
#   left_join(serogroups,by="RecordID") %>%
#   rename("SeroconversionGroup" = group) %>%
#   mutate(SeroconversionGroup = ifelse(SeroconversionGroup=="Neg.",NA,SeroconversionGroup))
# 
# nrow(Participant)
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
# rm(serogroups)
# 
# saveRDS(sourceData,'Data/sourceData.rds')