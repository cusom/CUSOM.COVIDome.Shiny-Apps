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




