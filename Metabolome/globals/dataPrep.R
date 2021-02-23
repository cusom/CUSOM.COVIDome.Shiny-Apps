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


