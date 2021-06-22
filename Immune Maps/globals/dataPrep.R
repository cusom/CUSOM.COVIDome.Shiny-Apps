Platforms <- getDataframeFromDatabase("[covidome].[GetApplicationPlatforms] ?",tibble("ApplicationID"= appConfig$applicationID)) %>%
  filter(!str_detect(Platform,"Live")) %>%
  arrange() %>%
  pull()

Platforms <- c("Live cells",Platforms)

recordIDs <- getDataframeFromDatabase("SELECT distinct RecordID FROM [covidome].[vw_Participant]",NULL) %>% 
  pull() 

sexes <- getDataframeFromDatabase("SELECT distinct Sex FROM [covidome].[vw_Participant]",NULL) %>% 
  pull()

ageGroups <- c("All","21 & Over")

covidPositiveRecords <- getDataframeFromDatabase("SELECT distinct [RecordID] FROM [covidome].[vw_Participant] WHERE [Status] = 'Positive'",NULL)

Queryplatforms <- getDataframeFromDatabase("[covidome].[GetQueryPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID)) %>%
  filter(str_detect(QueryPlatform,"Live")) %>%
  filter(str_detect(QueryPlatform,"Immune")) %>%
  pull()

Comparisonplatforms <- getDataframeFromDatabase("[covidome].[GetComparisonPlatforms] ?",tibble("ApplicationID"=appConfig$applicationID)) %>%
  arrange(ComparisonPlatform) %>%
  pull() 

