sourceData <- readRDS('Data/sourceData.rds')

recordIDs <- unique(sourceData$RecordID)

gene <- sourceData %>%
  distinct(Analyte) %>%
  pull(Analyte) %>%
  sort()

genes <- as.data.table(gene)
rm(gene)

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
# 
# glimpse(sourceData)
# 
# sourceData$Specimen_type <- "Whole Blood"
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
#   mutate(AgeGroup = case_when(Age == "Under 21" ~ "Under 21", Age != "Under 21" ~ "21 & Over")) %>%
#   select(RecordID,Sex,AgeGroup,Status) %>%
#   unique()
# 
# sourceData <- sourceData %>%
#   inner_join(Participant,by="RecordID") %>%
#   mutate(MeasuredValue = RPKM, Measurement = 'RPKM') %>%
#   select(-c(RPKM))
# 
# rm(Participant)
# rm(metadata)
# 
# saveRDS(sourceData,'Data/sourceData.rds')



