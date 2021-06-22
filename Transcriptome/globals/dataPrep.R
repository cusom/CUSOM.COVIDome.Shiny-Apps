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

genes <-getDataframeFromDatabase("SELECT AnalyteName as [Analyte]  FROM [covidome].[Analyte] (nolock) WHERE [AllDataRNA] = 1",NULL) %>%
  select(Analyte) %>%
  arrange(Analyte) %>%
  unique() %>%
  pull() %>%
  data.table()

setnames(genes,"Gene")





