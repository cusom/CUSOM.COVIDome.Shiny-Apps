# suppress warnings  
storeWarn <- getOption("warn")
options(warn = -1)

# get App metadata
appConfig <- read_tsv('./config/appConfig.tsv')

ApplicationURL <- ifelse(appConfig$Environment=="Production",appConfig$applicationURL,'')

sideBarMenuItems <- read_tsv('./config/sideBarMenuItems.tsv') %>% filter(IsHidden == 0) %>% select(-c(IsHidden))

namespaces <- as.list(sideBarMenuItems$tabName) 

tabs <- as.list(sideBarMenuItems$tabName)    

dropdownlinks <- read_tsv("./config/dropdownlinks.tsv")

tutorials <- read_tsv("./config/tutorials.tsv")

plotlyCustomIcons <- readRDS('config/plotlyCustomIcons.rds')

statTests <- CUSOMShinyHelpers::getStatTestByKeyGroup.methods

adjustmentMethods <- c("none","Bonferroni","Benjamini-Hochberg (FDR)")

pValueThreshold <- 0.05



set.seed(round(runif(1,0,1)*1000000))


