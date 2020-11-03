# get App metadata
ApplicationId <- config::get(file = "config/config.yml", "applicationID")

appConfig <- readRDS('config/appConfig.rds')

appConfig$footerHTML <- CUSOMShinyHelpers::getSOMStandardFooter('images/medicine_h_clr.png')

ApplicationURL <- ifelse(appConfig$Environment=="Production",'','')

sideBarMenuItems <- readRDS('config/sidebarMenuItems.rds')

namespaces <- as.list(sideBarMenuItems$tabName) 

tabs <- as.list(sideBarMenuItems$tabName)    

dropdownlinks <- read.csv("config/dropdownlinks.csv")

tutorials <- read.csv("config/tutorials.csv")

plotlyCustomIcons <- readRDS('config/plotlyCustomIcons.rds')

statTests <- CUSOMShinyHelpers::getStatTestByKeyGroup.methods

adjustmentMethods <- c("none","Bonferroni","Benjamini-Hochberg (FDR)")

pValueThreshold <- 0.05

# suppress warnings  
storeWarn <- getOption("warn")
options(warn = -1)

### paletes
d21Colors <- c("#BBBDC0","#f2f2f3") 
t21Colors <- c("#1D4D7C", "#3E99CD") 


set.seed(round(runif(1,0,1)*1000000))
