Platforms <- CUSOMShinyHelpers::getDataframeFromDatabase("[covidome].[GetApplicationPlatforms] ?",tibble("ApplicationID"= appConfig$applicationID), conn_args = conn_args) %>%
  filter(!str_detect(Platform,"Live")) %>%
  arrange() %>%
  pull()

Platforms <- c("Live cells",Platforms)

recordIDs <- CUSOMShinyHelpers::getDataframeFromDatabase("SELECT distinct RecordID FROM [covidome].[vw_Participant]",NULL,conn_args = conn_args) %>% 
  pull() 

sexes <- CUSOMShinyHelpers::getDataframeFromDatabase("SELECT distinct Sex FROM [covidome].[vw_Participant]",NULL,conn_args = conn_args) %>% 
  pull()

ageGroups <- c("All","21 & Over")

covidPositiveRecords <- CUSOMShinyHelpers::getDataframeFromDatabase("SELECT distinct [RecordID] FROM [covidome].[vw_Participant] WHERE [Status] = 'Positive'",NULL,conn_args = conn_args)

Queryplatforms <- CUSOMShinyHelpers::getDataframeFromDatabase("[covidome].[GetQueryPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID),conn_args = conn_args) %>%
  filter(str_detect(QueryPlatform,"Live")) %>%
  filter(str_detect(QueryPlatform,"Immune")) %>%
  pull()

Comparisonplatforms <- CUSOMShinyHelpers::getDataframeFromDatabase("[covidome].[GetComparisonPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID),conn_args = conn_args) %>%
  arrange(ComparisonPlatform) %>%
  pull() 

QueryAnalytes <- CUSOMShinyHelpers::getDataframeFromDatabase("SELECT AnalyteName as [Analyte] FROM [covidome].[Analyte] (nolock) WHERE [CorrelationImmune] = 1",NULL,conn_args=conn_args) %>%
  select(Analyte) %>%
  arrange(Analyte) %>%
  unique() %>%
  pull() %>%
  data.table()

setnames(QueryAnalytes,"Cell Population")