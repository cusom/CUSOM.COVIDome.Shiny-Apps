sourceData <- readRDS('Data/sourceData.rds')

platforms <- sort(unique(sourceData$Platform))

recordIDs <- unique(sourceData$RecordID)

sexes <- sourceData %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- c("All","21 & Over")

proteins <- sourceData %>%
  filter(grepl('Mass',Platform)) %>%
  distinct(Analyte) %>%
  pull(Analyte) %>%
  sort() %>%
  data.table()

setnames(proteins, "Protein : Swiss-Prot ID")

aptamers <- sourceData %>%
  filter(grepl('SOMA',Platform)) %>%
  distinct(Analyte) %>%
  pull(Analyte) %>%
  sort() %>%
  data.table()

setnames(aptamers, "Protein : SOMAmer ID")



