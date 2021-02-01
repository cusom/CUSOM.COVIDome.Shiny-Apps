sourceData <- readRDS('Data/sourceData.rds')

recordIDs <- unique(sourceData$RecordID)

platforms <- unique(sourceData$Platform)

genes <- sourceData %>%
  distinct(Analyte) %>%
  pull(Analyte) %>%
  sort() %>%
  data.table()

setnames(genes,"Gene")

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")

#######################
# library(dplyr)
# sourceData <- read.delim(file.choose(),stringsAsFactors=FALSE)
# serogroups <- read.delim(file.choose(), stringsAsFactors = FALSE)
# metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)
# 
# nrow(sourceData)
# nrow(serogroups)
# nrow(metadata)
# 
# glimpse(sourceData)
# 
# sourceData <- sourceData %>%
#   rename("Platform" = Specimen_type) %>%
#   mutate(Platform = "Whole Blood")
# 
# duplicateGenes <- sourceData %>%
#   group_by(Gene_name,RecordID) %>%
#   summarise(n = n()) %>%
#   filter(n > 1) %>%
#   ungroup() %>%
#   select(Gene_name) %>%
#   unique() %>%
#   pull()
# 
# sourceData <- sourceData %>%
#   mutate(Analyte = case_when(Gene_name %in% duplicateGenes ~ paste0(Gene_name,'|',gsub('ENSG00000','',EnsemblID)), !Gene_name %in% duplicateGenes ~ Gene_name))
# 
# rm(duplicateGenes)
# 
# ## should return 0 row tibble
# sourceData %>%
#   group_by(Analyte,RecordID) %>%
#   summarise(n=n()) %>%
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
#   mutate(MeasuredValue = RPKM, Measurement = 'RPKM') %>%
#   select(-c(RPKM))
# 
# rm(Participant)
# rm(metadata)
# rm(serogroups)
# 
# saveRDS(sourceData,'Data/sourceData.rds')



