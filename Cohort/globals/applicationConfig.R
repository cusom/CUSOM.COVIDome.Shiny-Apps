# suppress warnings  
storeWarn <- getOption("warn")
options(warn = -1)

# get App metadata
appConfig <- config::get(file = "config/config.yml","appConfig") 

isProductionApp <- ifelse(appConfig$Environment=="Production",TRUE,FALSE)

isDeployed <-  Sys.getenv('SHINY_PORT') != ""

ApplicationURL <- ifelse(appConfig$Environment=="Production",appConfig$applicationURL,'')

sideBarMenuItems <- read_tsv('./config/sidebarMenuItems.tsv',col_types = cols()) %>% filter(IsHidden == 0) %>% select(-c(IsHidden))

namespaces <- as.list(sideBarMenuItems$tabName) 

tabs <- as.list(sideBarMenuItems$tabName)    

dropdownlinks <- read_tsv("./config/dropdownlinks.tsv",col_types = cols())

