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
    tags$head(includeHTML("www/google-analytics.html")),
    tags$head(HTML('<meta name="robots" content="noindex">')),
    tags$head(HTML('<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>')),
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
  
  