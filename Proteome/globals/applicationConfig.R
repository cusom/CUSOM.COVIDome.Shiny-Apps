# get App metadata
ApplicationId <- config::get(file = "config/config.yml", "applicationID")

appConfig <- readRDS('config/appConfig.rds')

appConfig$footerHTML <- CUSOMShinyHelpers::getSOMStandardFooter('images/medicine_h_clr.png')

##### Global Constants ##### 
pValueThreshold <- 0.05

# suppress warnings  
storeWarn <- getOption("warn")
options(warn = -1)

### paletes
d21Colors <- c("#BBBDC0","#f2f2f3") 
t21Colors <- c("#1D4D7C", "#3E99CD") 

platformPlaceholder <- "Choose a platform..."
cytokinePlaceholder <- "Choose a cytokine..."
proteinPlaceholder <- "Choose a protein..."

set.seed(round(runif(1,0,1)*1000000))
