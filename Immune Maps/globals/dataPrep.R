sourceData <- readRDS('Data/sourceData.rds')

Platforms <- sourceData %>%
  group_by(Platform) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Platform) %>%
  ungroup() %>%
  pull()

recordIDs <- unique(sourceData$RecordID)

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")


