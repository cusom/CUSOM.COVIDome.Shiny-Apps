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
