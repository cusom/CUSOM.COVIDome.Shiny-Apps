# suppress warnings  
storeWarn <- getOption("warn")
options(warn = -1)

# get App metadata
appConfig <- read_tsv('./config/appConfig.tsv',col_types = cols())

isProductionApp <- ifelse(appConfig$Environment=="Production",TRUE,FALSE)

isDeployed <-  Sys.getenv('SHINY_PORT') != ""

ApplicationURL <- ifelse(appConfig$Environment=="Production",appConfig$applicationURL,'')

sideBarMenuItems <- read_tsv('./config/sidebarMenuItems.tsv',col_types = cols()) %>% filter(IsHidden == 0) %>% select(-c(IsHidden))

namespaces <- as.list(sideBarMenuItems$tabName) 

tabs <- as.list(sideBarMenuItems$tabName)    

dropdownlinks <- read_tsv("./config/dropdownlinks.tsv",col_types = cols())

tutorials <- read_tsv("./config/tutorials.tsv",col_types = cols())

plotlyCustomIcons <- readRDS('config/plotlyCustomIcons.rds')

statTests <- CUSOMShinyHelpers::getStatTestByKeyGroup.methods

adjustmentMethods <- c("none","Bonferroni","Benjamini-Hochberg (FDR)")



