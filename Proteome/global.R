# load required packages 
source('dependencies.R')

# load global objects
GlobalFileList <- list.files(path="globals/", pattern="*.R",full.names = TRUE, recursive = TRUE) 
sapply(GlobalFileList,source,.GlobalEnv)

# load all modules 
ModuleFileList <- list.files(path="R/", pattern="*.R",full.names = TRUE, recursive = TRUE) 
sapply(ModuleFileList,source,.GlobalEnv)

#cleanup
rm(GlobalFileList)
rm(ModuleFileList)


