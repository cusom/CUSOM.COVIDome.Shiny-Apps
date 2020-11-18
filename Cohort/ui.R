ui <- dashboardPagePlus( 
  collapse_sidebar = FALSE,
  title = appConfig$applicationName,
  header = dashboardHeaderPlus(
    title = tags$a(href=ApplicationURL,
                     tags$img(src='./images/logo.png', height='30'),
                     appConfig$projectName, 
                     style = "color:#000000;"), 
    titleWidth =  appConfig$titleWidth,
    left_menu = CUSOMShinyHelpers::createApplicationLinks(dropdownlinks)
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
    tags$head(includeHTML(("www/google-analytics.html"))),
    tags$head(HTML('<meta name="robots" content="noindex">')),
    tags$head(HTML("<script type='text/javascript' src='appHelpers.js'></script>")),
    tags$head(tags$link(rel="stylesheet",type = "text/css", href = "style.css")),
    tags$link(rel = "icon", href = "favicon.png"),
    useShinyjs(),
    introjsUI(),
    do.call(tabItems,map(namespaces, CUSOMShinyHelpers::createTabOutput))
  ), 
  skin = "blue-light"
)
  