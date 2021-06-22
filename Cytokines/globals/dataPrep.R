platforms <- getDataframeFromDatabase("[covidome].[GetApplicationPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID)) %>%
  arrange() %>%
  pull()

recordIDs <- getDataframeFromDatabase("SELECT distinct RecordID FROM [covidome].[vw_Participant]",NULL) %>% 
  pull() 

sexes <- getDataframeFromDatabase("SELECT distinct Sex FROM [covidome].[vw_Participant]",NULL) %>% 
  pull()

ageGroups <- c("All","21 & Over")

covidPositiveRecords <- getDataframeFromDatabase("SELECT distinct [RecordID] FROM [covidome].[vw_Participant] WHERE [Status] = 'Positive'",NULL)

Queryplatforms <- getDataframeFromDatabase("[covidome].[GetQueryPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID)) %>%
  arrange(QueryPlatform) %>%
  pull() 

Comparisonplatforms <- getDataframeFromDatabase("[covidome].[GetComparisonPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID)) %>%
  arrange(ComparisonPlatform) %>%
  pull() 

cytokines <- getDataframeFromDatabase("SELECT AnalyteName as [Analyte]  FROM [covidome].[Analyte] (nolock) WHERE [AllDataMSD] = 1",NULL) %>%
  select(Analyte) %>%
  arrange(Analyte) %>%
  unique() %>%
  pull() %>%
  data.table()

setnames(cytokines, "Cytokine")

Querycytokines <- getDataframeFromDatabase("SELECT AnalyteName as [Analyte]  FROM [covidome].[Analyte] (nolock) WHERE [CorrelationMSD] = 1",NULL) %>%
  select(Analyte) %>%
  arrange(Analyte) %>%
  unique() %>%
  pull() %>%
  data.table()

setnames(Querycytokines, "Cytokine")
