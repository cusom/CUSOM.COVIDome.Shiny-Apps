######################## REFRESH SOURCE DATA #####################

library(dplyr)
library(stringr)
MassSpec <- read.delim(file.choose(),stringsAsFactors=FALSE)
SOMA <- read.delim(file.choose(),stringsAsFactors=FALSE)
serogroups <- read.delim(file.choose(), stringsAsFactors = FALSE)
metadata <- read.delim(file.choose(),stringsAsFactors=FALSE)

nrow(MassSpec)
nrow(SOMA)
nrow(serogroups)
nrow(metadata)

setdiff(colnames(MassSpec),colnames(SOMA))
setdiff(colnames(SOMA),colnames(MassSpec))

SOMA$Sex <- NULL
SOMA$Status <- NULL
SOMA$Visit <- NULL
SOMA$raw_relative_abundance <- NA
SOMA$Platform <- "SOMAscan"
MassSpec$Specimen_type <- "Plasma"
MassSpec$Aptamer <- NA
MassSpec$GeneSymbol <- NA
MassSpec$Platform <- "Mass spectrometry"
colnames(SOMA)[which(colnames(SOMA)=="TargetFullName")] <- "Description"
colnames(MassSpec)[which(colnames(MassSpec)=="swissprotID")] <- "UniProt"

setdiff(colnames(MassSpec),colnames(SOMA))
setdiff(colnames(SOMA),colnames(MassSpec))

SOMA$Description <- paste0(SOMA$Analyte,': ',SOMA$Description)
SOMA$Analyte <- str_replace(SOMA$Aptamer, "\\.", ":")  #SOMA$Aptamer

MassSpec$Analyte <- paste0(MassSpec$Analyte,':',MassSpec$UniProt)

sourceData <- bind_rows(SOMA,MassSpec)

## sanity checks
## should == 0
(nrow(MassSpec) + nrow(SOMA)) - nrow(sourceData)

## should = original row counts
sourceData %>%
  group_by(Platform) %>%
  summarise(n = n())

## should return 0 row tibble
sourceData %>%
  group_by(Platform,Analyte,RecordID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

rm(SOMA)
rm(MassSpec)

Participant <- metadata %>%
  select(RecordID,Sex,Age,Status) %>%
  mutate(AgeGroup = case_when(Age < 21 ~ "Under 21", Age >= 21  ~ "21 & Over")) %>%
  select(RecordID,Sex,Age,AgeGroup,Status) %>%
  unique()

nrow(Participant)

Participant <- Participant %>%
  left_join(serogroups,by="RecordID") %>%
  rename("SeroconversionGroup" = group) %>%
  mutate(SeroconversionGroup = ifelse(SeroconversionGroup=="Neg.",NA,SeroconversionGroup))

nrow(Participant)

sourceData <- sourceData %>%
  inner_join(Participant,by="RecordID") %>%
  mutate(MeasuredValue = adjusted_relative_abundance, Measurement = 'adjusted relative abundance') %>%
  select(-c(adjusted_relative_abundance))

rm(Participant)
rm(metadata)
rm(serogroups)

saveRDS(sourceData,'Data/sourceData.rds')
