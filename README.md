# CUSOM.COVIDome.Shiny-Apps
Shiny Applications for COVIDome Project

## Overview 

## Applications 
Clinical 
>   Clinical data is presented in multiple formats for exploration. 
---
  Metabolome 
  > This dashboard presents metabolite data 
--- 
Proteome 
  > This dashboard contains protein expression data generated from plasma 

---
Transcriptome
> This dashboard presents RNA sequencing expression data generated from various sample types
---
Immune Map
  > This dashboard contains data generated from peripheral blood using two different platforms: flow-cytometry and mass-cytometry. 

## Application Requirements 
### The following required to get started with R and Shiny:

### R runtime https://www.r-project.org/
  - Windows Install https://cran.r-project.org/bin/windows/base/
  - Mac https://cran.r-project.org/bin/macosx/

### R IDE (R Studio recommended)
- R Studio https://rstudio.com/

## Application Project Structure
```
└───<AppName>
    │-- ui.R
    │-- server.R
    |-- global.R
    |-- dependencies.R
    └───globals
        |-- globalScript1.R
        |-- globalScript2.R
        |-- globalScriptN.R
    └───R
        └───Modules 
            |-- moduleScript1.R
            |-- moduleScript2.R
            |-- moduleScriptN.R
        └───Utilities
            |-- utilityScript1.R
            |-- utilityScript2.R
            |-- utilityScriptN.R
    └───Data 
        |-- dataFile1
        |-- dataFile2
        |-- dataFileN
    └───config
        |-- config.yaml 
        |-- configDataframe1
        |-- configDataframe2
    └───www
        |-- app_logo.png
        |-- appHelpers.js
        |-- style.css
        └───images
            |--image1
            |--image2
            |--imageN
    └───packrat
        |--init.R
        |--packrat.lock
        |--packrat.opts 
    └───rsconnect
        └───shinyapps.io
            └───<accountName>
                |-- <appname>.dcf      
    |--.Rprofile
    |--<AppName>.RProj
```

### Basic Application Functionality 
#### global.R
`global.r` 

````r
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
````
#### ui.r

````r
ui <- dashboardPagePlus( 
  collapse_sidebar = FALSE,
  header = dashboardHeaderPlus(
    title = tags$a(href=ApplicationHomeURL,
                   tags$img(src='app_logo.png', height='30'),
                   '<ApplicationGroupName>', 
                   style = "color:#fff;"), 
    titleWidth =  appConfig$titleWidth,
    left_menu = dropdownItems
  ),  
  sidebar = dashboardSidebar(
    collapsed = FALSE,
    width = appConfig$sidebarWidth,
    sidebarMenu(
       id="sidebar"
      ,pmap(sideBarMenuItems, createMenuItem)
      ,tags$br()
      ,map(namespaces, createSidebarInputs)
      )
    ), 
  body = dashboardBody(
    tags$head(HTML('<meta name="robots" content="noindex">')),
    tags$head(HTML("<script type='text/javascript' src='appHelpers.js'></script>")),
    tags$style(HTML(read_file(normalizePath(file.path('./www/style.css'))))),
    useShinyjs(),
    do.call(tabItems,map(namespaces, createTabOutput))
  )
)
````
#### server.R
`server.r` 
``` r 
server <- function(input, output, session) {
  map(namespaces,createServerObject)  
}
```

### www  
The www directory contains web assets sourced by the application. this includes images, html files, and custom CSS.

### Packrat
#### Package Requirements 
These projects use `Packrat` to manage package dependencies. 
> https://www.r-project.org/nosvn/pandoc/packrat.html

> https://rstudio.github.io/packrat/

`Packrat` will generate a `packrat.lock` file under the `packrat` directory that includes the specific package requirements for the project:
```` .lock
Package: BH
Source: CRAN
Version: 1.69.0-1
Hash: 15f597ed227897f4f793b6161260f4b9

Package: DT
Source: CRAN
Version: 0.6
Hash: 0ce85f84c88eec265dc0292df098b3c6
Requires: crosstalk, htmltools, htmlwidgets, magrittr, promises

Package: RODBC
Source: CRAN
Version: 1.3-15
Hash: 79cddc0b6f14b128f3b67a48c2bffd2f
````
#### Collaboration Workflow
The basic workflow to collaborating with projects that have package dependencies is as follows:

1. Clone the project locally. 
2. Ensure you have installed the `packrat` package. Run the following `r` script in the console:
```` r 
install.packages("packrat")
````
3. Populate the local package library. Run the following `r` script in the console:
 ```` r
packrat::restore()
 ````

#### Packages Used 
The following packages are used throughout the project, and almost in each of the major applications:

| Package Name     | Description   | URL  |
| :------------- |:-------------| :-----|
shiny | base package for shiny|https://cran.r-project.org/web/packages/shiny/shiny.pdf|
DT | data tables (JS) |https://rstudio.github.io/DT/|
tidyverse | dplyr, ggplot, etc.|https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf|
plotly | data visualization pacakge|https://plot.ly/r/|
shinydashboard | base dashboard skeleton (header, sidebar, body, etc.)|https://rstudio.github.io/shinydashboard/|
shinyWidgets | extension of UI inputs and tools|https://cran.r-project.org/web/packages/shinyWidgets/shinyWidgets.pdf|
shinycssloaders | shows spinners for loading UI elements|https://cran.r-project.org/web/packages/shinycssloaders/README.html|
shinyjs |Perform common useful JavaScript operations |https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf | 
formattable | extension of formatting for data tables package |https://cran.r-project.org/web/packages/formattable/formattable.pdf|
config |Manage configuration values across multiple environments |https://cran.r-project.org/web/packages/config/config.pdf|
crosstalk | Provides building blocks for allowing HTML widgets to communicate with each other|https://rstudio.github.io/crosstalk/|


### rsconnect 
In order to deploy these applications to shinyapps.io, you will need to define a publishing connection:

#### Configuring Publishing 
1. In any project, open the publishing configurations: Tools > Global Options > Publishing 
2. Click on the "Connect" button to configure a new publishing account. 
3. Choose the "ShinyApps.io" option. 
4. Get authorization token from shinyapps.io
- log into your shinyaps.io account > Account > Tokens 
- copy the existing token or add one as needed. 
5. Paste in the shinyapps.io token in the provided dialog box. 

### config 
#### Application Configurations
`config.yaml` 
config.yaml
``` YAML

```
This file is ignored in  `.gitignore` so you will need to create one in each project directory after cloning the repo. 



