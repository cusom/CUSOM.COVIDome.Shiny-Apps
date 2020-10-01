sourceData <- readRDS('Data/sourceData.rds')

ParticipantEncounter <- readRDS('Data/ParticipantEncounter.rds')

Participant <- ParticipantEncounter %>%
  select(RecordID,Sex,Age,Status) %>% 
  mutate(AgeGroup = case_when(Age == "Under 21" ~ "Under 21", Age != "Under 21" ~ "21 & Over")) %>%
  select(RecordID,Sex,AgeGroup,Status) %>%
  unique()

sourceData <- sourceData %>%
  select(-c(Status,Sex)) %>%
  inner_join(Participant,by="RecordID") %>%
  mutate(MeasuredValue = adjusted_relative_abundance, Measurement = 'adjusted relative abundance') %>%
  select(-c(adjusted_relative_abundance))

sexes <- Participant %>%
  select(Sex) %>%
  unique() %>%
  pull()

ageGroups <- Participant %>%
  select(AgeGroup) %>%
  unique() %>%
  pull()

analytes <- sourceData %>%
  select(Analyte) %>%
  unique() %>%
  pull()

rm(Participant)
rm(ParticipantEncounter)

######################## 
# msd <- sourceData %>%
#   filter(grepl('Meso',Platform))
# 
# soma <- sourceData %>%
#   inner_join(
#     sourceData %>%
#       filter(!grepl('Meso',Platform)) %>%
#       select(Analyte) %>%
#       unique() %>%
#       mutate(k = round(runif(n(),min=0,max=1))) %>%
#       filter(k == 1) %>%
#       slice(1:100)
#     ,by="Analyte") 
# 
# sourceData <- bind_rows(msd, soma)
# 
# saveRDS(sourceData,'./Data/sourceData.rds')
# 
# ParticipantEncounter <- sourceData %>%
#   select(LabID) %>%
#   unique() %>%
#   mutate(record_id = str_replace(LabID,'HTP',''))  %>%
#   mutate(record_id = str_remove(record_id,'[A,B]')) %>%
#   mutate(l = str_length(record_id)) %>%
#   mutate(record_id = case_when(l == 4 ~ record_id, l > 4 ~ substr(record_id,0,4))) %>%
#   select(LabID, record_id) %>%
#   unique() 
# 
# 
# ParticipantEncounter <- ParticipantEncounter %>%
#   inner_join(
#     ParticipantEncounter %>%
#       select(record_id) %>%
#       unique() %>%
#       mutate(csf = as.integer(record_id) %% 2) %>%
#       mutate(CovidStatus = ifelse(csf==0,"Negative","Positive")) %>%
#       mutate(Age = round(runif(n(),min=5,max=100))) %>%
#       mutate(AgeGroup = case_when(Age <= 18 ~"Under 18",Age > 18 ~"Over 18")) %>%
#       mutate(SexFlag = round(runif(n(),min=0,max(1)))) %>%
#       mutate(Sex = ifelse(SexFlag==0,"Male","Female")) %>%
#       select(-c(csf,Age,SexFlag)) 
#     ,by="record_id")
# 
# 
# 
# ParticipantEncounter %>%
#   group_by(record_id) %>%
#   summarize(n = n_distinct(Sex)) %>%
#   filter(n >1)
# 
# ParticipantEncounter %>%
#   group_by(record_id) %>%
#   summarize(n = n_distinct(AgeGroup)) %>%
#   filter(n >1)
# 
# ParticipantEncounter %>%
#   group_by(record_id) %>%
#   summarize(n = n_distinct(CovidStatus)) %>%
#   filter(n >1)
# 
# saveRDS(ParticipantEncounter,'./Data/ParticipantEncounter.rds')
