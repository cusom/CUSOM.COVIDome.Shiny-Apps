# CUSOM.COVIDome.Shiny-Apps
COVIDome Explorer Applications

## Overview 
The COVIDome Explorer is the result of a highly collaborative effort aims to generate multidimensional datasets from biospecimens from COVID-19 patients and controls, which will be integrated with matching clinical data. Following open science principles, the COVIDome datasets will be made accessible to the community through an online research portal, the COVIDome Explorer, as a global resource for data mining and hypothesis generation. Our mission is to enable the development of better prevention, diagnostic, and therapeutic tools for the clinical management of COVID-19.

This repository contains the code and artifacts required to run the COVIDome Explorer applications. 

## Applications 
Cohort 
>   This dashboard presents the introduction to the COVIDome Explorer. 
---
Cytokines 
>   This dashboard presents proteomic data related to cytokines 
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
  └───config
    |-- appConfig.tsv
    |-- dropdownlinks.tsv
    |-- plotlyCustomIcons.rds
    |-- sidebarMenuItems.tsv
    |-- tutorials.tsv
  └───Data 
    |-- sourceData.rds
    |-- dataFile1
    |-- dataFile2
    |-- dataFileN
  └───globals
    |-- globalScript1.R
    |-- globalScript2.R
    |-- globalScriptN.R
  └───packrat
    |--init.R
    |--packrat.lock
    |--packrat.opts 
  └───R
    └───Modules 
      |-- moduleScript1.R
      |-- moduleScript2.R
      |-- moduleScriptN.R        
  └───rsconnect
    └───shinyapps.io
      └───<accountName>
        |-- <appname>.dcf         
  └───www
    |-- favicon.png
    |-- appHelpers.js
    |-- google-analytics.html
    |-- style.css
    |-- misc_file1
    |-- misc_file2
    |-- misc_fileN
    └───images
      |--logo.png
      |--image1
      |--image2
      |--imageN

  |--.Rprofile
  |--<AppName>.RProj
```
## Basic Application Functionality 
### global.R
The `global.R` file will be the first script called when the application is initialized. This script is standard across all applications. It will perform 3 major tasks to initialize the application:

1) Load all required package dependencies
2) Run all global scripts found in the globals directory 
3) Run all module scripts found in the R directory 

``` r 
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
```
### ui.r
The `ui.R` file defines the structure of the entire application. This script is standard across all applications. Several lines of code will dynamically create UI elements by reading configurations and items read into memory from module files:

* application `title` property is read from the application configuration 
* `application url` property is read from the application configuration 
* `application logo` is always read from the `www` directory 
* application `title-width` property  is read from the application configuration 
* external `application links` (nav bar across the top of the application), are created by reading application configuration (dropdown links object) and invoking a helper function from the `CUSOMShinyhelpers` library: `createApplicationLinks`
* application `sidebar-width` property is read from the application configuration
* `sidebar tabs` are created by reading application configuration (sideBarMenuITems object) and invoking a helper function from the `CUSOMShinyhelpers` library: `createMenuItem`
* application `inputs` that are mapped to specfic side bar items are created by reading application configuration (namespeces object) and invoking a helper function from the `CUSOMShinyhelpers` library: `createSidebarInputs`
* application sidebar tab content is dynamically created by reading application configurations (tabItems and namespaces objects) and invoking a helper function from the `CUSOMShinyhelpers` library: `createTabOutput`. 
* `google-analytics` scripts are conditionally added to the application based on configuration (is the app deployed? is the app a production version?)
* custom javascript functions are always loaded from the `www` root
* custom application css styles are always loaded from the `www` root
* standard application `footer` is loaded by invoking a helper function from the `CUSOMShinyhelpers` library: `getSOMStandardFooter`. 

```r
ui <- dashboardPagePlus( 
  collapse_sidebar = FALSE,
  title = appConfig$applicationName,
  header = dashboardHeaderPlus(
    title = tags$a(href=ApplicationURL,
                   tags$img(src='./images/logo.png', height='30'),
                   appConfig$projectName, 
                   style =  "color:#000000;"),
    titleWidth =  appConfig$titleWidth,
    left_menu = CUSOMShinyHelpers::createApplicationLinks(dropdownlinks), 
    dropdownMenu(
      type = "messages",
      badgeStatus = NULL,
      icon = icon("fas fa-question-circle fa-lg", class = "header-dropdown-icon"),
      headerText = "Application Information",
      tags$li(
        a(href = "https://github.com/cusom/CUSOM.COVIDome.Shiny-Apps",
          target = "_blank",
          tagAppendAttributes(
            icon("fab fa-github"), class = "text-info"),
          "COVIDome Explorer Project"
          ), 
        p("Documentation, Source, Citation")
      )
    )
  ),
  
  sidebar = dashboardSidebar(
    collapsed = FALSE,
    width = appConfig$sidebarWidth,
    sidebarMenu(
       id="sidebar"
      ,pmap(sideBarMenuItems, CUSOMShinyHelpers::createMenuItem)
      ,tags$br()
      ,map(namespaces, CUSOMShinyHelpers::createSidebarInputs)
      )
    ),
  
  body = dashboardBody(
    ifelse(isDeployed && isProductionApp,tags$head(includeHTML(("www/google-analytics.html"))),''),
    tags$head(HTML('<meta name="robots" content="noindex">')),
    tags$head(HTML("<script type='text/javascript' src='appHelpers.js'></script>")),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);"),
    tags$head(tags$link(rel="stylesheet",type = "text/css", href = "style.css")),
    tags$link(rel = "icon", href = "favicon.png"),
    useShinyjs(),
    introjsUI(),
    do.call(tabItems,map(namespaces, CUSOMShinyHelpers::createTabOutput))
  ), 
  footer = dashboardFooter(
    HTML(
      CUSOMShinyHelpers::getSOMStandardFooter('images/brand_config_amc03.png','Created in partnership between the Office of the Vice Chancellor for Research and the School of Medicine')
      )
    ),
  skin = "blue-light" 
)
```
### server.R
The `server.R` file dynamically creates server objects by reading module objects loaded into memory  (namespaces -> which map to module outputs) and invoking a helper function from the `CUSOMShinyhelpers` library: `createServerObject`. 
``` r 
server <- function(input, output, session) {
  map(namespaces,createServerObject)  
}
```

### R Modules  
The `R` directory contains module files which are the core functionality for the applications. Modules are namespaced and any input or output will be bound to that namespace in the resulting application. Each module consists of 2 functions required to create desired functionality in the final application:

1) UI function 

The UI function returns 2 nammed lists:
1) Inputs - define the namespaced widget inputs to drive reactivity within the module 
2) Outputs - defines the UI output structure for the module 
``` r 
<Namespace>UI <- function(id) {
  list(
    "Inputs" = ...
  ), 
  list(
    "Outputs" = ...
  )
}
``` 

2) Server Function 
- The Server function performs all server-side processing and logic bound to the namespaced elements defined in the UI function. It utilizes a sub function `moduleServer` to bind the appropriate namespace (`id`)
``` r 
<Namespace>Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ...
  }
}
```

### www  
The www directory contains web assets sourced by the application. this includes images, html files, and custom CSS.

Custom Javascript 

Application Styling 

Google Analytics 


### Application configuration
#### Application Configurations

Dropdown Links 

Sidebar Menu Items 

Tutorials 

plotly Custom Icons 

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

## Collaboration Workflow
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

### Core Packages Used 
The following packages are used throughout the project, and almost in each of the major applications:

| Package Name     | Description   | URL  |
| :------------- |:-------------| :-----|
shiny | base package for shiny|https://cran.r-project.org/web/packages/shiny/shiny.pdf|
CUSOMShinyHelpers | Common Shiny Helper Functions |https://github.com/cusom/CUSOM.ShinyHelpers
tidyverse | dplyr, ggplot, etc.|https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf|
plotly | data visualization pacakge|https://plot.ly/r/|
shinydashboard | base dashboard skeleton (header, sidebar, body, etc.)|https://rstudio.github.io/shinydashboard/|
shinyWidgets | extension of UI inputs and tools|https://cran.r-project.org/web/packages/shinyWidgets/shinyWidgets.pdf|
shinyjs |Perform common useful JavaScript operations |https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf | 


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







