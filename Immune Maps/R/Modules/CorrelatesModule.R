CorrelatesUI <- function(id) {
  list(
    "Inputs" = 
      list(
        div(
          id=NS(id,"Dataset-Options"),class="sidebar-text",
          HTML(
            glue(
              '<h4>Set Dataset Options
                <span  
                  data-toggle="tooltip" 
                  data-placement="auto right" 
                  title="" 
                  class="fas fa-filter"
                  data-original-title="Set options below to generate volcano plot">
                </span>
              </h4>'
            )
          )
        )
        ,actionButton(
            NS(id,"PrimaryTutorial"), 
            label = "Take Tutorial", 
            class ="tutorial-btn", 
            icon = icon("question-circle")
        )
        ,bsTooltip(
          id = NS(id,"PrimaryTutorial"),
          title = "Click here to learn about setting dataset options to generate the volcano plot",
          placement = "top",
          trigger = "hover"
        )
        ,CUSOMShinyHelpers::createInputControl(
          controlType = "radioButtons", 
          inputId = NS(id,"Platform"),
          label = "1) Select Query Platform",
          choices = sort(Queryplatforms), 
          selected = Queryplatforms[1]
        )
        ,shinyjs::hidden(
          CUSOMShinyHelpers::createInputControl(
            controlType = "pickerInput", 
            inputId = NS(id,"StatTest"),
            label = "Statistical Test", 
            choices = statTests[grepl("Spearman",statTests)], 
            selected = statTests[grepl("Spearman",statTests)], 
            inline=FALSE 
          )
        ),
        shinyjs::hidden(
          CUSOMShinyHelpers::createInputControl(
            controlType = "pickerInput", 
            inputId = NS(id,"AdjustmentMethod"),
            label = "Adjustment Method", 
            choices = adjustmentMethods,
            selected = adjustmentMethods[3], 
            inline=FALSE
          )
        ),
        shinyjs::hidden(
          CUSOMShinyHelpers::createInputControl(
            controlType = "checkboxGroupInput", 
            inputId = NS(id,"Sex"),
            label = "Sex", 
            choices = sexes,
            selected = sexes, 
            inline=TRUE 
          )
          ,CUSOMShinyHelpers::createInputControl(
            controlType = "radioButtons", 
            inputId = NS(id,"AgeGroup"),
            label = "Age Group", 
            choices = ageGroups,
            selected = ageGroups[1], 
            inline=TRUE 
          )        
        ),
        div(
          id=NS(id,"QueryAnalyteInput"),
          selectizeInput(
            inputId = NS(id,"QueryAnalyte"),
            label="2) Select Query Analyte",
            choices= NULL,
            options = list(
              placeholder = 'Please select below',
              onInitialize = I('function() { this.setValue(""); }'), 
              closeAfterSelect = TRUE, 
              selectOnTab = TRUE, 
              persist = FALSE, 
              `live-search` = TRUE, 
              maxoptions = 1
            )
          )
        )
        ,div(
          id=NS(id,"ComparisonPlatformInput"),
          selectizeInput(
            inputId = NS(id,"ComparisonPlatform"),
            label="3) Choose Comparison Platform",
            choices= Comparisonplatforms,
            options = list(
              placeholder = 'Choose Comparison Platform',
              onInitialize = I('function() { this.setValue(""); }'), 
              closeAfterSelect = TRUE, 
              selectOnTab = TRUE, 
              persist = FALSE, 
              `live-search` = TRUE, 
              maxoptions = 1
            )
          )
        )
        ,div(class = "refresh-btn-label", "4) Generate Volcano Plot")
        ,actionButton(
          NS(id,"VolcanoDatasetRefresh"), 
          label = "Generate Volcano Plot", 
          class = "refresh-btn", 
          icon = icon("play")
        )
        ,div(
          id=NS(id,"AnalyteInput"),
          selectizeInput(
            inputId = NS(id,"ComparisonAnalyte"),
            label="Analyte",
            choices= NULL,
            options = list(
              placeholder = '5) Please select below',
              onInitialize = I('function() { this.setValue(""); }'), 
              closeAfterSelect = TRUE, 
              selectOnTab = TRUE, 
              persist = FALSE, 
              `live-search` = TRUE, 
              maxoptions = 1
            )
          )         
        )        
        ,tags$hr() 
        ,shinyjs::hidden(
          div(
            id = NS(id,"ExternalLinks"), class="sidebar-text-overflow"
            ,htmlOutput(
              NS(id,"ExternalLinksText")
            )              
            ,actionLink(
              inputId = NS(id,"Pubmed"), 
              label = "Pubmed", 
              icon = icon("external-link-alt")
            )
            ,actionLink(
              inputId = NS(id,"GeneCards"), 
              label = "GeneCards",
              icon = icon("external-link-alt")
            )
            ,actionLink(
              inputId = NS(id,"GTEx"), 
              label = "GTEx",
              icon = icon("external-link-alt")
            )
            ,actionLink(
              inputId = NS(id,"NCBI"), 
              label = "NCBI",
              icon = icon("external-link-alt")
            )
            ,actionLink(
              inputId = NS(id,"Wikipedia"), 
              label = "Wikipedia", 
              icon = icon("external-link-alt")
            )
          )
        )
        ,shinyjs::hidden(
          selectizeInput(
            NS(id,"TutorialName"),
            label="TutorialName",
            choices=c("DatasetOptions","VolcanoPlot","BoxPlot","BoxplotGroupComparison","Other"),
            options = list(
              placeholder = '',
              onInitialize = I('function() { this.setValue(""); }'),
              closeAfterSelect = TRUE,
              selectOnTab = TRUE,
              persist = FALSE,
              `live-search` = TRUE,
              maxoptions = 1
            )
          )
        )
      ), 
    
    "Outputs" = 
      list(
        tabsetPanel(
          tabPanel(
            title = uiOutput(
              NS(id,"PlotsTitle")
            ),   
            fluidRow(
              column(
                width=6,
                div(
                  id = NS(id,"VolcanoContent"),
                  boxPlus(
                    title = htmlOutput(
                      NS(id,"VolcanoPlotTitle")
                    ),
                    height = "auto",
                    width = "auto",
                    closable = FALSE,
                    status = "primary",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    withLoader(
                      plotlyOutput(
                        NS(id,"VolcanoPlot"),
                        height = "678px"
                      ), 
                      type = "html",
                      loader = "dnaspin"
                    )
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = NS(id,"VolcanoContentEmpty"),
                    boxPlus(
                      id = NS(id,"VolcanoContentEmptyBox"),
                      title = htmlOutput(
                        NS(id,"VolcanoContentEmptyTitle")
                      ),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,                       
                      withSpinner(
                        uiOutput(
                          NS(id,"VolcanoEmptyText"),
                          height = "630px"
                        )
                      )                   
                    ) 
                  )
                ), 
                shinyjs::hidden(
                  div(
                    id = NS(id,"VolcanoStart"),
                    boxPlus(
                      id = NS(id,"VolcanoStartBox"),
                      title = htmlOutput(
                        NS(id,"VolcanoStartTitle")
                      ),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,                       
                      withSpinner(
                        uiOutput(
                          NS(id,"VolcanoStartText"),
                          height = "630px"
                        )
                      )                   
                    ) 
                  )
                ), 
                div(
                  id = NS(id,"VolcanoTutorialStart")
                  ,style = "padding-left: 10%;"
                  ,actionButton(
                    NS(id,"SecondaryTutorial"), 
                    label = "Take Tutorial", 
                    class ="tutorial-btn", 
                    icon = icon("question-circle")
                  )
                  ,bsTooltip(
                    id = NS(id,"SecondaryTutorial"),
                    title = "Click here to learn about setting dataset options to generate the volcano plot",
                    placement = "top",
                    trigger = "hover"
                  )
                )
              ),
              column(
                width=6,
                  div(
                    id = NS(id,"AnalyteContent"),
                    boxPlus(
                      id = NS(id,"AnalyteContent"),
                      title = htmlOutput(
                        NS(id,"AnalyteBoxPlotPlotTitle")
                      ),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,                                               
                      withSpinner(
                        plotlyOutput(NS(id,"AnalytePlot"),
                        height = "658px")
                      )                   
                    )                   
                  ), 
                  shinyjs::hidden(
                  div(
                    id = NS(id,"AnalyteContentEmpty"),
                    boxPlus(
                      id = NS(id,"AnalyteContentEmptyBox"),
                      title = htmlOutput(NS(id,"BoxplotAnalyteEmptyTitle")),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,                       
                      withSpinner(uiOutput(NS(id,"BoxplotAnalyteEmptyText"),height = "630px"))                   
                    ) 
                  )
                )
              )
            )   
            ),
          tabPanel(title = uiOutput(NS(id,"FoldChangeDataTitle")),
            fluidRow(
              box(
                height="auto",
                width = 12,
                  fluidRow(
                    box(
                      width = 3,
                      height = 150,
                      sliderInput(
                        inputId = NS(id,"rho"),
                        label = HTML("Filter by rho"),
                        min = -1,
                        max = 1,
                        step = 0.05,
                        value = c(-1,1)
                      )
                    ), 
                    box(
                      width = 4,
                      height = 150,
                      radioGroupButtons(
                        inputId =  NS(id,"PValue"),
                        label = "Filter by q-value significance level",
                        choices = c("all"," * q &le; 0.1", " ** q &le; 0.01", " *** q &le; 0.001"),
                        individual = FALSE,
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                          no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")
                        )
                      )          
                    ) 
                  ),
                tags$hr(),
                fluidRow(
                  box(
                    title = "",
                    id = NS(id,"tabpanel2"),
                    height="auto",
                    width = 11,
                    withSpinner(DT::dataTableOutput(NS(id,"FoldChangeDataTable")))
                  )
                )
              )
            )
          ),
          tabPanel(title = uiOutput(NS(id,"SelectedAnalyteRawDataTitle")),
            id = NS(id,"AnalyteDataTablePanel"),
            fluidRow(
              box(
                title = "",
                id = NS(id,"AnalyteDataTablePanelBox"),
                height="auto",
                width = 11,
                withSpinner(DT::dataTableOutput(NS(id,"AnalyteDataTable")))
              )
            )                                   
          )        
        )
      )
  ) 
}

CorrelatesServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    ### labels / parameters   
    volcanoTopAnnotationLabel <- 'Rho'

    ### Reactive Values #
    rv <- reactiveValues(
      RunRefresh = -1, 
      Platform = "",
      ComparisonPlatform = "",
      maxFoldChange = 0,
      lastGroupFilled = NA ,
      queryAnalyte = list(
        name = '', 
        searchName = '' 
      ),
      comparisonAnalyte = list(
        name = '', 
        searchName = '' 
      ),
      selectedAnalyte = list(
       pvalue = NA, 
       xCoordiante = NA, 
       yCoordinate = NA, 
       name = '', 
       searchName = '', 
       comparisonAnalyte = ''
      ),
      tutorialClicks = list(
       "BoxplotGroupComparison" = 0
      ),
      ignoreTutorial= 0, 
      errorMessage = "",
      groupselectmodalstate = -1  
    )  

    observeEvent(input$PrimaryTutorial,{

      updateSelectizeInput(
        session = session,
        inputId = "TutorialName",
        selected = "DatasetOptions"
      )

    })

  
    observeEvent(input$SecondaryTutorial,{

      updateSelectizeInput(
        session = session,
        inputId = "TutorialName",
        selected = "DatasetOptions"
      )

    })

    observeEvent(input$TutorialName, {
      
      #keep track of times each tutorial has been shown...
      if(input$TutorialName=="BoxplotGroupComparison") {  

        rv$tutorialClicks$BoxplotGroupComparison <- rv$tutorialClicks$BoxplotGroupComparison + 1
        
        if(rv$tutorialClicks$BoxplotGroupComparison > 1) {

          rv$ignoreTutorial <- 1

        }

      } 

      else {
        
        rv$ignoreTutorial <- 0
        
      }

      if(input$TutorialName != '' & rv$ignoreTutorial == 0) {
        
        introjs(session = session, options=list(steps=tutorialSteps()))
        
        updateSelectizeInput(
          session = session,
          inputId = "TutorialName",
          selected = ""
        )
        
      }
      
    },ignoreInit=TRUE)     
 
    tutorialSteps <- reactive({
      
      tutorials %>%
        filter(namespace==id) %>%
        filter(tutorialName==input$TutorialName)

    })
       
    observeEvent(c(input$VolcanoDatasetRefresh),{
      
      rv$RunRefresh <- rv$RunRefresh + 1
      
      shinyjs::show("VolcanoContent")
      shinyjs::hide("VolcanoContentEmpty")
      shinyjs::hide("AnalyteContentEmpty")
      
    },ignoreInit=TRUE)   
    
    observeEvent(c(input$Platform),{

      rv$analyteLabel <- "Cell Population"
      rv$analytePlaceholder <- "Cell Population"
      AnalyteLabel <- "2) Select Cell Population"

      updateSelectizeInput(
        session = session,
        inputId = "QueryAnalyte",
        label = AnalyteLabel,
        choices = QueryAnalytes, 
        options = list(
          placeholder = rv$analytePlaceholder,
          onInitialize = I('function() { this.setValue(""); }'), 
          closeAfterSelect = TRUE, 
          selectOnTab = TRUE, 
          persist = FALSE, 
          `live-search` = TRUE, 
          maxoptions = 1
        )
      )
      
      shinyjs::hide("LogTransform")
      shinyjs::hide("GroupAnalysisOptions")
      shinyjs::hide("AnalyteContent")
      shinyjs::hide("ExternalLinks")

    })

    observeEvent(c(input$QueryAnalyte),{
      
      if(input$QueryAnalyte == "") {
        
        shinyjs::disable(id = "ComparisonPlatform")
        
        shinyjs::runjs(glue("Plotly.purge('{id}-VolcanoPlot');"))
        shinyjs::hide("VolcanoContent")
        shinyjs::show("VolcanoStart")
        shinyjs::show("VolcanoTutorialStart")
        
        shinyjs::runjs(glue("Plotly.purge('{id}-AnalytePlot');"))
        shinyjs::hide("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::hide("ExternalLinks")

        updateSelectizeInput(
          session = session,
          inputId = "ComparisonAnalyte",
          label = "5) Comparison Analyte (optional)",
          selected = "",
          options = list(
            placeholder = 'Choose a comparison analyte',
            onInitialize = I('function() { this.setValue(""); }'),
            closeAfterSelect = TRUE,
            selectOnTab = TRUE,
            persist = FALSE,
            `live-search` = TRUE,
            maxoptions = 1
          )
        )

        updateSelectizeInput(
          session = session,
          inputId = "ComparisonPlatform",
          label = "3) Choose Comparison Platform",
          choices = Comparisonplatforms,
          selected = "",
          options = list(
            placeholder = 'Choose Comparison Platform',
            onInitialize = I('function() { this.setValue(""); }'),
            closeAfterSelect = TRUE,
            selectOnTab = TRUE,
            persist = FALSE,
            `live-search` = TRUE,
            maxoptions = 1
          )
        )


      }
      
      if(rv$queryAnalyte$name != input$QueryAnalyte && rv$queryAnalyte$name != "") {
        
        shinyjs::runjs(glue("Plotly.purge('{id}-VolcanoPlot');"))
        shinyjs::hide("VolcanoContent")
        shinyjs::show("VolcanoStart")
        shinyjs::show("VolcanoTutorialStart")
        
        shinyjs::runjs(glue("Plotly.purge('{id}-AnalytePlot');"))
        shinyjs::hide("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::hide("ExternalLinks")
        
        updateSelectizeInput(
          session = session,
          inputId = "ComparisonPlatform",
          label = "3) Choose Comparison Platform",
          choices = Comparisonplatforms,
          selected = "",
          options = list(
            placeholder = 'Choose Comparison Platform',
            onInitialize = I('function() { this.setValue(""); }'),
            closeAfterSelect = TRUE,
            selectOnTab = TRUE,
            persist = FALSE,
            `live-search` = TRUE,
            maxoptions = 1
          )
        )
        
      }
      
      if(input$QueryAnalyte != "") {
        
        shinyjs::enable(id = "ComparisonPlatform")
        
      }
             
      rv$queryAnalyte$name <- input$QueryAnalyte
      
    })

    observeEvent(c(input$ComparisonPlatform),{
      
      if(rv$ComparisonPlatform != input$ComparisonPlatform && rv$ComparisonPlatform != "") {

        shinyjs::disable("VolcanoDatasetRefresh")
        shinyjs::addClass(id = "VolcanoDatasetRefresh", class = "refresh-ready-btn")

        updateSelectizeInput(
          session = session,
          inputId = "ComparisonAnalyte",
          label = "5) Comparison Analyte (optional)",
          choices = "", 
          options = list(
            placeholder = 'Choose a comparison analyte',
            onInitialize = I('function() { this.setValue(""); }'), 
            closeAfterSelect = TRUE, 
            selectOnTab = TRUE, 
            persist = FALSE, 
            `live-search` = TRUE, 
            maxoptions = 1
          )
        )
        
        shinyjs::runjs(glue("Plotly.purge('{id}-VolcanoPlot');"))
        shinyjs::hide("VolcanoContent")
        shinyjs::show("VolcanoStart")
        shinyjs::show("VolcanoTutorialStart")

        shinyjs::runjs(glue("Plotly.purge('{id}-AnalytePlot');"))
        shinyjs::hide("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::hide("ExternalLinks")       

      }
           
      if (input$ComparisonPlatform != "") {
       
        show_modal_spinner(
          spin = "half-circle",
          color = "#3c8dbc",
          text = glue("Getting list of {input$ComparisonPlatform} comparison analytes...Please wait...")
        )

        shinyjs::disable("ComparisonAnalyte")

        q <- "[covidome].[GetComparisonAnalytes] ?, ?, ?"
        p <- tibble("QueryPlatform" =  input$Platform, "QueryAnalyte" = input$QueryAnalyte, "ComparisonPlatform" = input$ComparisonPlatform)
      
        comparisonAnalytes <- CUSOMShinyHelpers::getDataframeFromDatabase(q,p,conn_args = conn_args) %>%
          arrange(ComparisonAnalyte) %>%
          pull()

        updateSelectizeInput(
          session = session,
          inputId = "ComparisonAnalyte",
          label = "5) Comparison Analyte (optional)",
          choices = comparisonAnalytes, 
          options = list(
            placeholder = 'Choose a comparison analyte',
            onInitialize = I('function() { this.setValue(""); }'), 
            closeAfterSelect = TRUE, 
            selectOnTab = TRUE, 
            persist = FALSE, 
            `live-search` = TRUE, 
            maxoptions = 1
          )
        )

        shinyjs::enable("VolcanoDatasetRefresh")
        shinyjs::addClass(id = "VolcanoDatasetRefresh", class = "refresh-ready-btn")
        shinyjs::enable("ComparisonAnalyte")
        shinyjs::enable("ComparisonPlatform")

        rv$ComparisonPlatform <- input$ComparisonPlatform

        remove_modal_spinner()

      }

      if(input$ComparisonPlatform == "") {
        
        shinyjs::disable("VolcanoDatasetRefresh")
        shinyjs::removeClass(id = "VolcanoDatasetRefresh", class = "refresh-ready-btn")
        shinyjs::disable("ComparisonAnalyte")
      
      }

    })
   
    # change plots tab title based on chosen platform
    output$PlotsTitle <- renderUI({
      glue('{input$Platform} Plots')
    })
        
    getCorrelationsDataset <- function(QueryPlatform, QueryAnalyte, ComparisonPlatform) {
    
      tryCatch(

        {
        
          q <- "[covidome].[GetCorrelationDataset] ?, ?, ?"
          
          p <- tibble("QueryPlatform" =  QueryPlatform, 
                      "QueryAnalyte" = QueryAnalyte, 
                      "ComparisonPlatform" = ComparisonPlatform
                      )

          correlationData <- CUSOMShinyHelpers::getDataframeFromDatabase(q,p,conn_args = conn_args)  
          
          max_finite <- correlationData %>%
            filter(p.value > 0) %$%
            min(p.value) %>%
            -log10(.)
          
          correlationData <- correlationData %>% 
            mutate(
              shape = if_else(p.value == 0, "infinite", "finite"),
              p.value = if_else(p.value == 0, 10^-(max_finite * 1.05), p.value), 
              `-log10pvalue` = -log10(p.value)
            )
          
          return(correlationData)
        },

        error=function() {
          return(NULL)
        }

      )

    }
      
    FoldChangeData <- eventReactive(c(input$VolcanoDatasetRefresh), {
     
      shinyjs::hide("VolcanoTutorialStart")
      shinyjs::hide("VolcanoStart")
      shinyjs::hide("AnalyteContent")
      shinyjs::hide("LogTransform")
      shinyjs::hide("ExternalLinks")
     
      if(input$ComparisonPlatform != "") {
        
        show_modal_spinner(
          spin = "atom",
          color = "#3c8dbc",
          text = "Getting correlation data...Please wait..."
        )
       
        correlationData <- getCorrelationsDataset(input$Platform, input$QueryAnalyte, input$ComparisonPlatform) %>%
          mutate(`selected_`= ifelse(ComparisonAnalyte==input$ComparisonAnalyte,1,0), 
                 "p.value.adjustment.method" = "BH") %>%
          drop_na()

        
        rv$errorMessage <- ""
        rv$Platform <- input$Platform
        rv$queryAnalyte$name <- input$QueryAnalyte
        rv$queryAnalyte$searchName <- CUSOMShinyHelpers::parseDelimitedString(input$QueryAnalyte,1) 
        rv$RunRefresh <- 0

        remove_modal_spinner()
  
        return(correlationData)

      }

      else {
        
        rv$errorMessage <- "Correlation Dataset not found. Please check error logs"
        return(NULL)

      }

    }, ignoreInit = TRUE)

    # change the fold change tab title based on platform
    output$FoldChangeDataTitle <- renderUI ({
      ifelse(input$ComparisonPlatform=="","Correlation Data",glue('{input$QueryAnalyte}-{input$ComparisonPlatform} Correlation Data'))
    })

    shared_FoldChangeData <- SharedData$new(FoldChangeData)

    FoldChangeDataTableData <- reactive({
     
      shared_FoldChangeData$data(withSelection = FALSE) %>%
        mutate(pvalueCutoff = case_when(input$PValue=="all" ~ 1 , input$PValue==" * q &le; 0.1" ~ 0.1, input$PValue==" ** q &le; 0.01" ~ 0.01 , input$PValue==" *** q &le; 0.001" ~ 0.001)) %>%
        filter(rho >= min(input$rho), rho <= max(input$rho)) %>%
        filter(p.value <= pvalueCutoff) %>%
        select(-c(selected_,p.value.original,pvalueCutoff,shape,p.value.adjustment.method)) %>%
        select(`Query Platform` = QueryPlatform,`Query Analyte` = QueryAnalyte,`Comparison Platform` = ComparisonPlatform, 
               `Comparison Analyte` = ComparisonAnalyte, rho, `q-value (BH Adjusted)` = p.value, `-log10 q-value`= `-log10pvalue`)
      
    })
    
    output$FoldChangeDataTable <- DT::renderDataTable({
    
      DT::datatable(
        data=FoldChangeDataTableData(),
        caption = htmltools::tags$caption(
           style = 'caption-side: bottom; text-align: center;',
           glue('{input$Platform}-{input$QueryAnalyte}-{input$ComparisonPlatform} Correlation Data')
        ),
        #filter = 'top',
        extensions = c('Buttons','ColReorder','Responsive','Scroller'),
        selection = 'none',
        options = list(
          dom = 'Brftip',
          colReorder = TRUE,
          autowidth=FALSE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')
        ),
        rownames = FALSE,
        style = 'bootstrap',
        escape = FALSE
      )
    }, server=FALSE)



    # Volcano Plot #### 
    output$VolcanoPlot <- renderPlotly({

      validate(
        need(rv$queryAnalyte$name != "", "")
      )
            
      dataframe <- shared_FoldChangeData$data(withSelection = FALSE) 
      
      if(!is.null(dataframe)) {     
        shinyjs::hide("VolcanoContentEmpty")
        shinyjs::hide("VolcanoTutorialStart")
        shinyjs::hide("VolcanoStart")
        shinyjs::show("VolcanoContent")      
        shinyjs::show("AnalyteContent")
        
        ## gets a list of annotations / shapes / parameters 
        a <- dataframe %>% 
          CUSOMShinyHelpers::getCorrelationVolcanoAnnotations(
            foldChangeVar = rho,
            pValueVar = `-log10pvalue`,
            selected = `selected_`,
            arrowLabelTextVar = ComparisonAnalyte,
            titleText = glue("Correlation with {rv$queryAnalyte$searchName}:") )
        
        dataframe %>%
          rowwise() %>%
          mutate(
            text = glue("{ComparisonAnalyte}
                        Rho:{round(rho,2)}
                        {CUSOMShinyHelpers::formatPValue(p.value,a$parameters$adjustmentMethod)}"
                        ) ) %>%
          
          CUSOMShinyHelpers::addSignificanceGroup(
            foldChangeVar = rho,
            pValueVar = `-log10pvalue`, 
            threshold = a$parameters$pValueThresholdTransformed ) %>%
          
          CUSOMShinyHelpers::getVolcanoPlot(
            foldChangeVar = rho,
            pValueVar = `-log10pvalue`,
            significanceGroup =  significanceGroup, 
            text = text, 
            key = ComparisonAnalyte, 
            plotName = id,
            shape = shape ) %>%

          layout(xaxis = list(title="rho",fixedrange = FALSE,zeroline=TRUE, zerolinewidth=1)) %>% 
          layout(yaxis = list(title=glue(ifelse(a$parameters$pValueAdjustedInd,"q-value ","p-value "),"(-log<sub>10</sub>)"),fixedrange = FALSE)) %>%
          layout(shapes=a$shapes) %>%
          layout(annotations=c(a$annotations,a$arrow)) %>%
          layout(margin = list( t = 60)) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue('{appConfig$applicationName} - Volcano Plot {format(Sys.time(),"%Y%m%d_%H%M%S") }'),
              width = session$clientData[[ glue('output_{id}-VolcanoPlot_width') ]],
              height = session$clientData[[ glue('output_{id}-VolcanoPlot_height') ]]
            ),
            modeBarButtons = list(
              list("zoom2d"),
              list("zoomIn2d"),
              list("zoomOut2d"),
              list("resetScale2d"),
              list("toImage") 
            )
          )  %>% 
          onRender("function(el) { overrideModebarDivId(el); }") 
      } 

      else {
        shinyjs::hide("VolcanoContent")
        shinyjs::show("VolcanoContentEmpty")
        CUSOMShinyHelpers::getBoxplotForEmptyData(text = "")
      }

    })

    output$VolcanoStartText <- renderUI({
      
      if(input$Platform != rv$Platform & rv$Platform != "") {
        message <- glue('It looks like you changed Platforms. Please set dataset options to refresh plot')
      }

      else {
        message <- glue('Please start by setting dataset options to generate volcano plot <br /> <br />
                        Once your options are set, click the "Generate Volcano Plot" button to see your results.')
      }
      
      HTML(
        message       
        )

    })

    output$VolcanoStartTitle <- renderUI({     
      HTML(
        glue(
         '<h3>Please start by setting dataset options
            <span onclick=\"launchTutorial(\'{id}\',\'DatasetOptions\')\"
            data-toggle="tooltip" 
            data-placement="auto right" 
            title="" 
            class="fas fa-info-circle gtooltip"
            style="color:#1e8bf0"
            data-original-title="Click here to learn about setting dataset options">
            </span>
          </h3>
          '        
        )
      )
    })

    output$VolcanoEmptyText <- renderUI({
      HTML(
        glue(
         'Based on your chosen filters, there are not enough observations to generate the plot. <br /> <br />
          Please re-adjust dataset options and try again.'
        )
      )
    })

    output$VolcanoPlotTitle <- renderUI({
     
      title <- ifelse(input$VolcanoDatasetRefresh,glue('Correlation between {CUSOMShinyHelpers::parseDelimitedString(input$QueryAnalyte,1)} and {input$ComparisonPlatform} in COVID-19'),'Please start by setting dataset options below')
      
      tutorial <- ifelse(input$VolcanoDatasetRefresh,'VolcanoPlot','DatasetOptions')
      
      tooltip <- ifelse(input$VolcanoDatasetRefresh,'Click here to learn about Spearman Correlation plots','Click here to learn about setting dataset options')
      
      HTML(
        glue(
          '<h3>{title} 
            <span onclick=\"launchTutorial(\'{id}\',\'{tutorial}\')\"
            data-toggle="tooltip" 
            data-placement="auto right" 
            title="" 
            class="fas fa-info-circle gtooltip"
            style="color:#1e8bf0"
            data-original-title="{tooltip}">
            </span>
          </h3>'
        )
      )

    })
    
    output$VolcanoContentEmptyTitle <- renderUI({
      HTML(
        glue(
          '<h3>Unable to display plot
            <span onclick=\"launchTutorial(\'{id}\',\'DatasetOptions\')\" 
              data-toggle="tooltip"
              data-placement="auto right" 
              title="" 
              class="fas fa-info-circle gtooltip"
              style="color:#1e8bf0"
              data-original-title="Click here to learn about setting dataset options">
            </span>
          </h3>'
        )
      )
    })
  
    #### Observe Volcano Plot Clicks ####
    observeEvent(event_data("plotly_click", source = glue("{id}VolcanoPlot")),{ 

      e <- event_data("plotly_click", source = glue("{id}VolcanoPlot"))
      
      updateSelectizeInput(
        session = session,
        inputId = "ComparisonAnalyte",
        selected = e$key
      )

    },ignoreInit=TRUE)    

    observeEvent(c(input$ComparisonAnalyte),{

      plotName <- glue("{id}-VolcanoPlot")

      if (input$ComparisonAnalyte != '') {
        
        rv$comparisonAnalyte$name <- input$ComparisonAnalyte
               
        rv$comparisonAnalyte$searchName <- CUSOMShinyHelpers::parseDelimitedString(input$ComparisonAnalyte,1)
        
        keys <- glue_collapse(input$ComparisonAnalyte,sep="|")
        shinyjs::runjs(glue('annotatePointByKey("{plotName}","{keys}",6);') )

        if(rv$RunRefresh==1) {

          shinyjs::show("AnalyteContent")
          shinyjs::hide("LogTransform")
          shinyjs::show("ExternalLinks")
          shinyjs::hide("AnalyteContentEmpty")

        }

      }

      else {

        rv$RunRefresh <- 0
        keys <- ''
        shinyjs::runjs(glue('annotatePointByKey("{plotName}","{keys}",6);') )

        if(rv$Platform != input$Platform) {
          shinyjs::hide("VolcanoContent")
          shinyjs::hide("AnalyteContent")
          shinyjs::hide("LogTransform")
          shinyjs::hide("ExternalLinks")
          shinyjs::show("VolcanoStart")
          shinyjs::show("VolcanoTutorialStart")
        }
      }

    })
   
    onclick("Pubmed",shinyjs::runjs(glue("window.open('https://www.ncbi.nlm.nih.gov/pubmed/?term={rv$comparisonAnalyte$searchName}',target = '_blank')")))

    onclick("GeneCards", shinyjs::runjs(glue("window.open('https://www.genecards.org/Search/Keyword?queryString={rv$comparisonAnalyte$searchName}',target = '_blank')")))

    onclick("GTEx", shinyjs::runjs(glue("window.open('https://www.gtexportal.org/home/gene/{rv$comparisonAnalyte$searchName}',target = '_blank')")))

    onclick("NCBI", shinyjs::runjs(glue("window.open('https://www.ncbi.nlm.nih.gov/gene/?term={rv$comparisonAnalyte$searchName}',target = '_blank')")))

    onclick("Wikipedia", shinyjs::runjs(glue("window.open('https://en.wikipedia.org/w/index.php?search={rv$comparisonAnalyte$searchName}',target = '_blank')")))

    output$ExternalLinksText <- renderUI({
      HTML(
        glue(
        '<h4>Search external sites <br />for {rv$comparisonAnalyte$searchName}
          <span 
            data-toggle="tooltip" 
            data-placement="auto right" 
            title="" 
            class="fas fa-info-circle gtooltip"
            style="color:#1e8bf0"
            data-original-title="Click any link below to search external sites for {rv$comparisonAnalyte$searchName}">
          </span>
        </h4>'
        )
      )
    })
    
    GetAnalyteDataByPlatform <- function(Platform,Analyte) {
      
      tryCatch(
        
        {
          
          q <- "[covidome].[GetAnalyteDataByPlatform] ?, ?, ?"
          p <- tibble("Platform" =  Platform, "Analyte" = Analyte,"ReturnAdjusted" = 1)
          
          AnalyteData <- CUSOMShinyHelpers::getDataframeFromDatabase(q,p,conn_args = conn_args)  
          
          return(AnalyteData)
          
        },
        
        error=function() {
          return(NULL)
        }
        
      )
    }
    

    AnalyteDataset <- reactive({ 
      
      validate(
        need(!is.na(input$QueryAnalyte),""),
        need(input$QueryAnalyte != "",""),
        need(!is.na(input$ComparisonAnalyte),""),
        need(input$ComparisonAnalyte != "","")
      )
      
      show_modal_spinner(
        spin = "orbit",
        color = "#3c8dbc",
        text = "Fetching data...Please wait..."
      )
     
      dataframe <- GetAnalyteDataByPlatform(input$ComparisonPlatform, input$ComparisonAnalyte) %>%
        filter(outlier==FALSE) %>%
        select(RecordID, MeasuredValue, Measurement) %>%
        rename(x = MeasuredValue) %>%
        inner_join(
          GetAnalyteDataByPlatform(input$Platform, input$QueryAnalyte) %>%
            filter(outlier==FALSE) %>%
            select(RecordID,  MeasuredValue, Measurement) %>%
            rename(y = MeasuredValue) 
          , by="RecordID"
        ) %>%
        mutate(log2x = log2(x), log2y = log2(y), 
               xLabel = CUSOMShinyHelpers::parseDelimitedString(input$ComparisonAnalyte,1),
               yLabel = CUSOMShinyHelpers::parseDelimitedString(input$QueryAnalyte,1)
        ) %>%
        inner_join(covidPositiveRecords, by="RecordID")
      
      remove_modal_spinner()
       
      if(!is.null(dataframe)) {
      
        dataframe  
      }

      else {
        NULL
      }

    })

    output$SelectedAnalyteRawDataTitle <- renderText({
      ifelse(input$ComparisonAnalyte=="","Scatterplot Data",glue('{input$ComparisonAnalyte} vs. {input$QueryAnalyte} Scatterplot Data'))
    })

    output$AnalyteDataTable <- DT::renderDataTable({
      
      dataframe <- AnalyteDataset()
      
      analyte_x <- dataframe$xLabel[1]
      measurement_x <- dataframe$Measurement.x[1]
      xlabel <- glue("{analyte_x} log<sub>2</sub> {measurement_x}")
      analyte_y <- dataframe$yLabel[1]
      measurement_y <- dataframe$Measurement.y[1]
      ylabel <- glue("{analyte_y} log<sub>2</sub> {measurement_y}")
      
      dataframe <- dataframe %>% 
        select(-c(Measurement.x, Measurement.y,xLabel,yLabel,x,y)) %>% 
        rename(`:=`(!!xlabel,log2x), `:=`(!!ylabel, log2y))
      
      DT::datatable(
        data = dataframe,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          ifelse(input$ComparisonAnalyte=="","Scatterplot Data",glue('{input$ComparisonAnalyte} vs. {input$QueryAnalyte} Scatterplot Data'))
        ),
        filter = 'top',
        extensions = c('Buttons','ColReorder','Responsive','Scroller'),
        options = list(
          dom = 'Bfrtip',
          colReorder = TRUE,
          autowidth=TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          scrollX =TRUE,
          columnDefs = list(list(width = '200px', targets = "_all")),
          pageLength = 10, 
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')
        ), 
        rownames = FALSE, 
        style = 'bootstrap',
        escape = FALSE
      )
    }, server=TRUE) 
  
    # Plot ####
    output$AnalytePlot <- renderPlotly({
      
      dataframe <- AnalyteDataset()
      
      if(nrow(dataframe)>0) {  
        
        shinyjs::show("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::show("ExternalLinks")
        shinyjs::hide("AnalyteContentEmpty")
        
        measurement.x <- dataframe[1,'Measurement.x']
        measurement.y <- dataframe[1,'Measurement.y']
        AnalyteLabel <- CUSOMShinyHelpers::parseDelimitedString(input$ComparisonAnalyte,1)  
        comparisonAnalyteLabel <- CUSOMShinyHelpers::parseDelimitedString(input$QueryAnalyte,1)  
        
        p <- dataframe %>%        
          mutate(Density = CUSOMShinyHelpers::getDensityColors(x, y, transform = TRUE)) %>% 
          arrange(Density) %>%
          ungroup() %>% 
          mutate(text = glue("{xLabel} log<sub>2</sub> {measurement.x}: {round(log2x,2)}<br>{yLabel} log<sub>2</sub> {measurement.y}: {round(log2y,2)}<br>Density: {Density}")) %>%
          CUSOMShinyHelpers::getScatterPlotWithSmoothing(
            xVar = log2x, 
            yVar = log2y, 
            colorVar = Density, 
            textVar =  text, 
            smoothingMethod = "lm"
          )
         
        ggplotly(p, tooltip="text") %>% 
          layout(
            title = list(
              text = '', 
              font = list(
                family = "Arial", 
                size = 24, 
                color = "#004F80"
              )
            ), 
            showlegend = FALSE, 
            legend = list(
              x = 100, 
              y = 0.1
            ),
            xaxis = list(
              title = glue("{AnalyteLabel} log<sub>2</sub> {measurement.x}") , 
               font = list(
                 family = "Arial", 
                 color = "rgb(58, 62, 65)", 
                 size = 18
                ), 
                showgrid = FALSE, 
                zeroline = FALSE, 
                showline = TRUE, 
                showticklabels = TRUE
            ),
            yaxis = list(
              title = glue("{comparisonAnalyteLabel} log<sub>2</sub> {measurement.y}"),
              font = list(
                 family = "Arial", 
                 color = "rgb(58, 62, 65)", 
                 size = 18
                ), 
                showgrid = FALSE, 
                zeroline = FALSE, 
                showline = TRUE, 
                showticklabels = TRUE
            ),
            font = list(
              family = "Arial", 
              color = "rgb(58, 62, 65)", 
              size = 18
            ), 
            margin = list(
              autoexpand = TRUE, 
              l = 25, 
              r = 15, 
              t = 20, 
              b = 20
            )
          ) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = glue('{appConfig$applicationName} - Analyte Plot {format(Sys.time(),"%Y%m%d_%H%M%S") }'),
              width = session$clientData[[ glue('output_{id}-AnalytePlot_width') ]],
              height = session$clientData[[ glue('output_{id}-AnalytePlot_height') ]]
            ),
            modeBarButtons = list(
              list("toImage") 
            )
          )  %>% 
          onRender("function(el) { overrideModebarDivId(el); }")
        
      }
      
      else {
        
        shinyjs::hide("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::hide("ExternalLinks")
        shinyjs::show("AnalyteContentEmpty")
        CUSOMShinyHelpers::getBoxplotForEmptyData(text = "")
        
      }

    })

    output$BoxplotAnalyteEmptyText <- renderUI({
      HTML(
        glue(
         'One of the groups chosen contains less than 10 samples. <br /> <br />
          Please re-adjust dataset options and try again.'
        )
      )
    })
        
    output$AnalyteBoxPlotPlotTitle <- renderUI({
    
      validate(
        need(!is.na(input$ComparisonAnalyte),""),
        need(input$ComparisonAnalyte != "","")
      )
      
      p <- shared_FoldChangeData$data(withSelection = FALSE) %>%
        filter(QueryAnalyte==input$QueryAnalyte, ComparisonAnalyte==input$ComparisonAnalyte) %>% 
        ungroup() %>%
        select(p.value,p.value.adjustment.method, rho) %>%
        distinct()
     
      if(nrow(p) > 0) {
        
        HTML(
          glue(
            '<h3>{CUSOMShinyHelpers::parseDelimitedString(input$ComparisonAnalyte,1)} vs {CUSOMShinyHelpers::parseDelimitedString(input$QueryAnalyte,1)} in COVID-19
              <span onclick=\"launchTutorial(\'{id}\',\'BoxPlot\')\"
                data-toggle="tooltip"
                data-placement="auto right" 
                title="" 
                class="fas fa-info-circle gtooltip"
                style="color:#1e8bf0"
                data-original-title="">
              </span>
            </h3>
            {CUSOMShinyHelpers::formatPValue(p$p.value,p$p.value.adjustment.method)} &nbsp;&nbsp;&nbsp;  rho = {round(p$rho,2)}'
          )
          
        )
        
      } 
      
      else {
        
        HTML(
          glue(
            '<h3>Please choose an analyte from the volcano plot
              <span onclick=\"launchTutorial(\'{id}\',\'VolcanoPlot\')\"
                data-toggle="tooltip"
                data-placement="auto right" 
                title="" 
                class="fas fa-info-circle gtooltip"
                style="color:#1e8bf0"
                data-original-title="">
              </span>
            </h3>'
          )
        )
      }
      
    })

    output$BoxplotAnalyteEmptyTitle <- renderUI({

      analyteLabel <- ifelse(input$ComparisonAnalyte!="", glue('for {input$ComparisonAnalyte}'),'')
      
      HTML(
        glue(
          '<h3>Unable to display plot {analyteLabel},
                  <span onclick=\"launchTutorial(\'{id}\',\'DatasetOptions\')\" 
                    data-toggle="tooltip"
                    data-placement="auto right" 
                    title="" 
                    class="fas fa-info-circle gtooltip"
                    style="color:#1e8bf0"
                    data-original-title="Click here to learn about setting dataset options">
                  </span>
                </h3>'
        )
      )
    })
         
  })
  
}