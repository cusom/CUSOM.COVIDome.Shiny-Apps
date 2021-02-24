KaryotypeUI <- function(id) {
  list(
    "Inputs" = 
      list(
        div(
          id=NS(id,"Dataset-Options"),class="sidebar-text",
          HTML(
            paste0(
              '<h3>Dataset Options 
                <span onclick=\"launchTutorial(\'',id,'\',\'DatasetOptions\')\" 
                  data-toggle="tooltip" 
                  data-placement="auto right" 
                  title="" 
                  class="fas fa-info-circle gtooltip"
                  data-original-title="Click here to learn about setting dataset options">
                </span>
              </h3>'
            )
          )
        )
        ,CUSOMShinyHelpers::createInputControl(controlType = "radioButtons", inputId = NS(id,"Platform"),label = "Platform",choices = sort(platforms), selected = platforms[1])
        ,CUSOMShinyHelpers::createInputControl(controlType = "radioButtons", inputId = NS(id,"StatTest"),label = "Statistical Test", choices = statTests, selected = statTests[1], inline=FALSE )
        ,CUSOMShinyHelpers::createInputControl(controlType = "radioButtons", inputId = NS(id,"AdjustmentMethod"),label = "Adjustment Method", choices = adjustmentMethods ,selected = adjustmentMethods[1], inline=FALSE )
        ,CUSOMShinyHelpers::createInputControl(controlType = "checkboxGroupInput", inputId = NS(id,"Sex"),label = "Sex", choices = sexes ,selected = sexes, inline=TRUE )
        ,CUSOMShinyHelpers::createInputControl(controlType = "radioButtons", inputId = NS(id,"AgeGroup"),label = "Age Group", choices = ageGroups ,selected = ageGroups[1], inline=TRUE )
        ,div(
          id=NS(id,"AnalyteInput"),
          selectizeInput(
            inputId = NS(id,"Analyte"),
            label="Analyte",
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
        ,actionButton(NS(id,"VolcanoDatasetRefresh"), "Apply filters and generate plot", class = "refresh-btn")
        ,tags$hr() 
        ,shinyjs::hidden(
          div(
            id = NS(id,"ExternalLinks"), class="sidebar-text-overflow"
            ,htmlOutput(NS(id,"ExternalLinksText"))              
            ,actionLink(inputId = NS(id,"Pubmed"), label = "Pubmed", icon = icon("external-link-alt"))
            ,actionLink(inputId = NS(id,"GeneCards"), label = "GeneCards",icon = icon("external-link-alt"))
            ,actionLink(inputId = NS(id,"GTEx"), label = "GTEx",icon = icon("external-link-alt"))
            ,actionLink(inputId = NS(id,"NCBI"), label = "NCBI",icon = icon("external-link-alt"))
            ,actionLink(inputId = NS(id,"Wikipedia"), label = "Wikipedia", icon = icon("external-link-alt"))
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
          tabPanel(title = uiOutput(NS(id,"PlotsTitle")),   
            fluidRow(
              column(
                width=6,
                div(
                  id = NS(id,"VolcanoContent"),
                  boxPlus(
                    title = htmlOutput(NS(id,"VolcanoPlotTitle")),
                    height = "auto",
                    width = "auto",
                    closable = FALSE, 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE,
                    withSpinner(plotlyOutput(NS(id,"VolcanoPlot"),height = "678px"))
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = NS(id,"VolcanoContentEmpty"),
                    boxPlus(
                      id = NS(id,"VolcanoContentEmptyBox"),
                      title = htmlOutput(NS(id,"VolcanoContentEmptyTitle")),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,                       
                      withSpinner(uiOutput(NS(id,"VolcanoEmptyText"),height = "630px"))                   
                    ) 
                  )
                ), 
                shinyjs::hidden(
                  div(
                    id = NS(id,"VolcanoStart"),
                    boxPlus(
                      id = NS(id,"VolcanoStartBox"),
                      title = htmlOutput(NS(id,"VolcanoStartTitle")),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,                       
                      withSpinner(uiOutput(NS(id,"VolcanoStartText"),height = "630px"))                   
                    ) 
                  )
                )
              ),
              column(
                width=6,
                  div(
                    id = NS(id,"AnalyteContent"),
                    boxPlus(
                      id = NS(id,"AnalyteContent"),
                      title = htmlOutput(NS(id,"AnalyteBoxPlotPlotTitle")),
                      height= "auto",
                      width = "auto",
                      closable = FALSE, 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE,         
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "primarySwitch", inputId = NS(id,"LogTransform"),label = HTML("Show as Log<sub>2</sub> Transformed?"), status="primary", value=TRUE)
                        ),
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"GroupA"),label = "Group A", choices = recordIDs, selected = NULL, multiple = TRUE )
                        ),
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"GroupB"),label = "Group B", choices = recordIDs, selected = NULL, multiple = TRUE )
                        ),                  
                      withSpinner(plotlyOutput(NS(id,"AnalyteBoxPlot"),height = "626px"))                   
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
            ,tags$hr()  
            ,fluidRow(
              shinyjs::hidden(
                div(
                  id = NS(id,"GroupAnalysisOptions"),
                  boxPlus(
                    title = "You have selected 2 groups! Compare them here:",
                    height = "auto",
                    width = "auto",
                    closable = FALSE, 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE,
                    fluidRow(
                      column(width=1),
                      column(
                        width=2,
                        fluidRow(
                          selectizeInput(
                            NS(id,"GroupAnalysisChoice"),
                            label = "Choose a comparison option below",
                            choices = c('To another analyte'),
                            options = list(
                              placeholder = 'Using the selected groups, compare',
                              onInitialize = I('function() { this.setValue(""); }'), 
                              closeAfterSelect = FALSE, 
                              selectOnTab = TRUE, 
                              persist = TRUE, 
                              `live-search` = TRUE, 
                              maxoptions = 1
                            )
                          )
                        ),
                        fluidRow(
                          selectizeInput(
                            NS(id,"AnalyteComparision"),
                            label="",
                            choices='',
                            options = list(
                              placeholder = 'Analytes available to compare to:',
                              onInitialize = I('function() { this.setValue(""); }'),
                              closeAfterSelect = FALSE,
                              selectOnTab = TRUE,
                              persist = TRUE,
                              `live-search` = TRUE,
                              maxoptions = 1
                            )
                          )
                        ),                       
                        tags$hr()                                       
                      ),
                      column(
                        width=8,
                        div(
                          withSpinner(plotlyOutput(NS(id,"AnalyteGroupComparisonPlot")))
                        )
                      ), 
                      column(1) 
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
                      inputId = NS(id,"FoldChange"),
                      label = HTML("Filter by Log<sub>2</sub> Fold Change"),
                      min = -10,
                      max = 10,
                      step = 0.05,
                      value = c(-10,10)
                    )
                  ), 
                  box(
                    width = 4,
                    height = 150,
                    radioGroupButtons(
                      inputId =  NS(id,"PValue"),
                      label = "Filter by p-value significance level",
                      choices = c("all"," * P &le; 0.05", " ** P &le; 0.01", " *** P &le; 0.001"),
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
          ),
          tabPanel(title = uiOutput(NS(id,"SelectedAnalyteHighlightGroupsDataTitle")),
            fluidRow(
              box(
                title = "",
                id = NS(id,"SelectedAnalyteRecordsDataTablePanel"),
                height="auto",
                width = 11,
                DT::dataTableOutput(NS(id,"SelectedAnalyteRecordsDataTable"))
              )
            )
          )
        )
    )
  ) 
}

KaryotypeServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    ### labels / parameters
    groupVariable <- "Status"
    baselineLabel <- "Negative"
    volcanoTopAnnotationLabel <- 'Up in COVID-19 +'
    groupVariableLabel <- "COVID-19 Status"
    
    ### Reactive Values #
    rv <- reactiveValues(RunRefresh = -1, 
                         Platform = "",
                         maxFoldChange = 0,
                         lastGroupFilled = NA ,
                         selectedAnalyte = list(
                           pvalue = NA, 
                           xCoordiante = NA, 
                           yCoordinate = NA, 
                           name = '', 
                           searchName = ''
                         ),
                         tutorialClicks = list(
                           "BoxplotGroupComparison" = 0
                         ),
                         ignoreTutorial= 0, 
                         errorMessage = "",
                         groupselectmodalstate = -1  
                         )  

    observeEvent(input$TutorialName, {
      
      updateSelectizeInput(
        session = session,
        inputId = "TutorialName",
        selected = input$TutorialName
      )

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
        
      }
      
    })   
      
 
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
      
    })
    
    observeEvent(c(input$Platform),{
      
      if(grepl('Mass',input$Platform)) {
        
        rv$analytePlaceholder <- "Protein : Swiss-Protein ID"
        AnalyteLabel <- "Highlight Protein (optional)"     
        rv$analyteLabel <- "Protein"
        analyteChoices <- proteins
        
      }
      
      if(grepl('SOMA',input$Platform)) {
        
        rv$analytePlaceholder <- "Protein : SOMAmer ID"
        AnalyteLabel <- "Highlight aptamer (optional)"
        rv$analyteLabel <- "Aptamer"
        analyteChoices <- aptamers

      }
      
      updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        label = AnalyteLabel,
        choices = analyteChoices, 
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
      
      updateSelectizeInput(
        session = session,
        inputId = "GroupAnalysisChoice", 
        selected = ""
      )

      updatePickerInput(
        session = session,
        inputId = 'GroupA',
        selected = ""
      )

      updatePickerInput(
        session = session,
        inputId = 'GroupB',
        selected = ""
      )    
      
      shinyjs::hide("LogTransform")
      shinyjs::hide("GroupAnalysisOptions")
      shinyjs::hide("AnalyteContent")
      shinyjs::hide("ExternalLinks")
     
    })
      
    # change plots tab title based on chosen platform
    output$PlotsTitle <- renderUI({
      paste0(input$Platform,' Plots')
    })
        
    # BASE DATASET WITH UI FILTERS APPLIED 
    dataWithFilters <- function(platform,sex,ageGroup) { 
      
      groupVariable <- enquo(groupVariable)
     
      dataframe <- sourceData %>%
        filter(Platform==platform) %>%
        filter(Sex %in% sex) %>%
        mutate(AgeGroupTemp = case_when(ageGroup=="All" ~ "All", ageGroup !="All" ~ AgeGroup)) %>%
        filter(AgeGroupTemp == ageGroup) %>%
        select(-AgeGroupTemp) %>%
        # Assign appropriate variable to more generic "Group Variable"
        rename(GroupVariable := !!groupVariable) %>%
        filter(!is.na(GroupVariable)) %>%
        CUSOMShinyHelpers::applyGroupCountThreshold(GroupVariable,Analyte, threshold = 1)
      
      if(nrow(dataframe) > 0) {
        return(dataframe)
      } 
      else {
        return(NULL)
      }
      
    }

    FoldChangeData <- eventReactive(c(input$VolcanoDatasetRefresh), {
      
      baseData <- dataWithFilters(input$Platform, input$Sex,input$AgeGroup)
     
      if(!is.null(baseData)) { 
          
        shinyjs::hide("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::hide("ExternalLinks")
        shinyjs::hide("VolcanoStart")
        
        show_modal_progress_circle(
            value = 0,
            text = "Generating Volcano Plot...",
            color = "#3c8dbc",
            stroke_width = 4,
            easing = "linear",
          
            trail_color = "#eee",
            trail_width = 1,
            height = "200px",
            session = shiny::getDefaultReactiveDomain()
          )
          
        update_modal_progress(
          value = 1,
          text = "Calculating Fold Change...",
          session = shiny::getDefaultReactiveDomain()
        )
              
        finalData <- baseData %>%
          mutate(log2MeasuredValue = ifelse(MeasuredValue==0,0,log2(MeasuredValue))) %>%
          mutate(GroupVariable = fct_relevel(GroupVariable, baselineLabel)) %>% # set ref level
          select(RecordID, Analyte, log2MeasuredValue, GroupVariable, Sex, Age) %>% 
          CUSOMShinyHelpers::getStatTestByKeyGroup(RecordID, Analyte, GroupVariable, baselineLabel, log2MeasuredValue, method = input$StatTest, adjustmentMethod = input$AdjustmentMethod, GroupVariable, Sex, Age) %>%
          mutate(selected_ = ifelse(Analyte==input$Analyte,1,0))

        update_modal_progress(
          value = 4,
          text = "Finalizing plot...",
          session = shiny::getDefaultReactiveDomain()
        )
          
        if("error" %in% colnames(finalData)) {
          
          rv$errorMessage <- paste0('Error running ',unique(finalData$method),': ',  unique(finalData$error))
          
          finalData <- NULL 
          
        }
        
        else {

          rv$errorMessage <- ""

        }

        remove_modal_progress(session = getDefaultReactiveDomain())
            
        rv$RunRefresh <- 0
        rv$selectedAnalyte$searchName <- str_split(input$Analyte, "\\:", simplify = TRUE)[1]
        rv$Platform <- input$Platform
        
        return(finalData)

      } 

      else {

        return(NULL)

      }   

    }, ignoreInit = TRUE)

    # change the fold change tab title based on platform
    output$FoldChangeDataTitle <- renderUI ({
      paste0(input$Platform,' Aggregated Data')
    })
    
    shared_FoldChangeData <- SharedData$new(FoldChangeData)
    
    FoldChangeDataTableData <- reactive({
      
      shared_FoldChangeData$data(withSelection = FALSE) %>%
        mutate(pvalueCutoff = case_when(input$PValue=="all" ~ 1 , input$PValue==" * P &le; 0.05" ~ 0.05, input$PValue==" ** P &le; 0.01" ~ 0.01 , input$PValue==" *** P &le; 0.001" ~ 0.001)) %>%
        mutate(method = input$StatTest) %>%
        filter(log2FoldChange >= min(input$FoldChange), log2FoldChange <= max(input$FoldChange)) %>%
        filter(p.value <= pvalueCutoff) %>%
        select(-c(selected_,pvalueCutoff)) %>%
        CUSOMShinyHelpers::formatFoldChangeDataframe(baselineLabel = baselineLabel)
      
    })
      
    output$FoldChangeDataTable <- DT::renderDataTable({
    
      DT::datatable(
        data=FoldChangeDataTableData(),
        caption = htmltools::tags$caption(
           style = 'caption-side: bottom; text-align: center;',
            'Fold Change Data: ', htmltools::em('Fold Change Data Used for Volcano Plot')
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
     
      dataframe <- shared_FoldChangeData$data(withSelection = FALSE) 
      
      if(!is.null(dataframe)) {
        
        ## gets a list of annotations / shapes / parameters 
        a <- dataframe %>% 
          CUSOMShinyHelpers::getVolcanoAnnotations(foldChangeVar = log2FoldChange,
                                                  pValueVar = `-log10pvalue`,
                                                  selected = `selected_`,
                                                  arrowLabelTextVar = Analyte,
                                                  upRegulatedText = volcanoTopAnnotationLabel
          )

        pValueSuffix <- ifelse(a$parameters$pValueAdjustedInd,"(adj) ","")

        shinyjs::show("VolcanoContent")
        shinyjs::hide("VolcanoContentEmpty")
        shinyjs::hide("VolcanoStart")
        shinyjs::show("AnalyteContent")

        dataframe %>%
          mutate(text = paste0(ifelse(grepl('Mass',input$Platform),"Protein:", "Aptamer:"), Analyte,
                              "<br />fold_change:", round(FoldChange,2),
                              "<br />p-value",pValueSuffix,": ",formatC(p.value, format = "e", digits = 2))) %>%
          
          CUSOMShinyHelpers::addSignificanceGroup(foldChangeVar = log2FoldChange,
                                                  pValueVar = `-log10pvalue`, 
                                                  threshold = a$parameters$pValueThresholdTransformed) %>%
          
          CUSOMShinyHelpers::getVolcanoPlot(foldChangeVar = log2FoldChange,
                                            pValueVar = `-log10pvalue`,
                                            significanceGroup =  significanceGroup, 
                                            text = text, 
                                            key = Analyte, 
                                            plotName = id) %>%
          
          layout(xaxis = list(title="Fold Change (log<sub>2</sub>)",fixedrange = FALSE)) %>%
          layout(yaxis = list(title=paste0("p-value ",pValueSuffix,"(-log<sub>10</sub>)"),fixedrange = FALSE)) %>%
          layout(shapes=a$shapes) %>%
          layout(annotations=c(a$annotations,a$arrow)) %>%
          layout(margin = list( t = 60)) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = paste0(appConfig$applicationName, " - Volcano Plot ",format(Sys.time(),"%Y%m%d_%H%M%S")) ,
              width = session$clientData[[paste0('output_',id,'-VolcanoPlot_width')]],
              height = session$clientData[[paste0('output_',id,'-VolcanoPlot_height')]]
            ),
            modeBarButtons = list(
              list("zoom2d"),
              list("zoomIn2d"),
              list("zoomOut2d"),
              list("resetScale2d"),
              list("toImage") 
            )
          ) %>% onRender("function(el) { overrideModebarDivId(el); }")

      } 

      else {

        shinyjs::hide("VolcanoContent")
        shinyjs::show("VolcanoContentEmpty")
        CUSOMShinyHelpers::getBoxplotForEmptyData(text = "")

      }

    })

    output$VolcanoStartText <- renderUI({
      
      if(input$Platform != rv$Platform & rv$Platform != "") {
        message <- paste0('It looks like you changed Platforms. Please set dataset options to refresh plot')
      }
      
      else {
        message <- paste0('Please start by setting dataset options to generate plot')
      }
      
      HTML(
        message       
        )

    })

    output$VolcanoStartTitle <- renderUI({
      
      HTML(
        paste0(
         '<h3>Please start by setting dataset options below
            <span onclick=\"launchTutorial(\'',id,'\',\'DatasetOptions\')\"
            data-toggle="tooltip" 
            data-placement="auto right" 
            title="" 
            class="fas fa-info-circle gtooltip"
            data-original-title="Click here to learn about setting dataset options">
            </span>
          </h3>'        
        )
      )
    })

    output$VolcanoEmptyText <- renderUI({
      HTML(
        paste0(
         'Based on your chosen filters, there are not enough observations to generate the plot. <br /> <br />
          Please re-adjust dataset options and try again.', 
          ifelse(rv$errorMessage!="",paste0('<br /><br /><em><b>',rv$errorMessage,'</b></em>'),'')
        )
      )
    })

    output$VolcanoPlotTitle <- renderUI({
     
      title <- ifelse(input$VolcanoDatasetRefresh,paste0('Effect of ',groupVariableLabel,' on all ',ifelse(grepl('Mass',input$Platform),'proteins','aptamers')),'Please start by setting dataset options below')
      
      tutorial <- ifelse(input$VolcanoDatasetRefresh,'VolcanoPlot','DatasetOptions')
      
      tooltip <- ifelse(input$VolcanoDatasetRefresh,'Click here to learn about volcano plots','Click here to learn about setting dataset options')
      
      HTML(
        paste0(
          '<h3>',title,' 
            <span onclick=\"launchTutorial(\'',id,'\',\'',tutorial,'\')\"
            data-toggle="tooltip" 
            data-placement="auto right" 
            title="" 
            class="fas fa-info-circle gtooltip"
            data-original-title="',tooltip,'">
            </span>
          </h3>'
        )
      )

    })
    
    output$VolcanoContentEmptyTitle <- renderUI({
      HTML(
        paste0(
          '<h3>Unable to display Volcano plot
            <span onclick=\"launchTutorial(\'',id,'\',\'DatasetOptions\')\" 
              data-toggle="tooltip"
              data-placement="auto right" title="" class="fas fa-info-circle gtooltip"
              data-original-title="Click here to learn about setting dataset options">
            </span>
          </h3>'
        )
      )
    })
  
    #### Observe Volcano Plot Clicks ####
    observeEvent(event_data("plotly_click", source = paste0(id,"VolcanoPlot")),{ 

      e <- event_data("plotly_click", source = paste0(id,"VolcanoPlot"))
      
      updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )

    })
    
    observeEvent(c(input$Analyte),{

      plotName <- paste0(id,"-VolcanoPlot")
     
      if (input$Analyte != '') {
        
        rv$selectedAnalyte$name <- input$Analyte
        rv$selectedAnalyte$searchName <- str_split(input$Analyte, "\\:", simplify = TRUE)[1]
                  
        keys <- paste0(input$Analyte,collapse = '|')       
        shinyjs::runjs(paste0('annotatePointByKey("',plotName,'","',keys,'",5);') )
       
        shinyjs::show("AnalyteContent")
        shinyjs::show("LogTransform")
        shinyjs::show("ExternalLinks")
        shinyjs::hide("AnalyteContentEmpty")  
      
      }
      
      else {

        keys <- ''
        shinyjs::runjs(paste0('annotatePointByKey("',plotName,'","',keys,'",5);') ) 
        shinyjs::runjs(paste0('clearSelectedPointsFromPlot("',plotName,'");') )
        shinyjs::hide("VolcanoContent")
        shinyjs::hide("AnalyteContent")
        shinyjs::hide("LogTransform")
        shinyjs::hide("ExternalLinks")
        shinyjs::show("VolcanoStart")
      }
      
    })
   
    onclick("Pubmed",shinyjs::runjs(paste0("window.open('https://www.ncbi.nlm.nih.gov/pubmed/?term=",rv$selectedAnalyte$searchName,"',target = '_blank')")))

    onclick("GeneCards", shinyjs::runjs(paste0("window.open('https://www.genecards.org/Search/Keyword?queryString=",rv$selectedAnalyte$searchName,"',target = '_blank')")))

    onclick("GTEx", shinyjs::runjs(paste0("window.open('https://www.gtexportal.org/home/gene/",rv$selectedAnalyte$searchName,"',target = '_blank')")))

    onclick("NCBI", shinyjs::runjs(paste0("window.open('https://www.ncbi.nlm.nih.gov/gene/?term=",rv$selectedAnalyte$searchName,"',target = '_blank')")))

    onclick("Wikipedia", shinyjs::runjs(paste0("window.open('https://en.wikipedia.org/w/index.php?search=",rv$selectedAnalyte$searchName,"',target = '_blank')")))

   output$ExternalLinksText <- renderUI({
      HTML(
        paste0(
        '<h4>Search external sites <br />for ',rv$selectedAnalyte$searchName,'
          <span 
            data-toggle="tooltip" 
            data-placement="auto right" 
            title="" 
            class="fas fa-info-circle gtooltip"
            data-original-title="Click any link below to search external sites for ',rv$selectedAnalyte$searchName,'">
          </span>
        </h4>'
        )
      )
    })

    getAnalyteDataset <- function(platform,sex,ageGroup,analyte,logTransform,groupA,groupB) {
      
      dataWithFilters(platform,sex,ageGroup) %>%
        filter(Analyte==analyte) %>%
        mutate(y = case_when(logTransform==TRUE ~ log2(MeasuredValue), logTransform==FALSE ~ MeasuredValue), 
              y_label = case_when(logTransform==TRUE ~ paste0("Log<sub>2</sub> ", Measurement), logTransform==FALSE ~ Measurement )) %>% 
        filter(y != Inf, y != -Inf) %>%
        mutate(highlightGroup = case_when(RecordID %in% groupA ~ "A", RecordID %in% groupB ~ "B"))    
      
    }

    AnalyteDataset <- reactive({ 
      
      validate(
        need(!is.na(input$Analyte),""),
        need(input$Analyte != "","")
      )
      
      dataframe <- getAnalyteDataset(input$Platform,input$Sex,input$AgeGroup,input$Analyte,input$LogTransform,input$GroupA,input$GroupB) 
      
      if(!is.null(dataframe)) {
       
        dataframe %>%         
          CUSOMShinyHelpers::applyGroupCountThreshold(GroupVariable, threshold = 10)       
      }

      else {
        NULL
      }

    })


    GetAnalyteData <- function(platform,sex,ageGroup,analyte,groupA,groupB) {
      
      dataframe <- dataWithFilters(platform,sex,ageGroup)

      if(analyte != "") {
        dataframe <- dataframe %>% filter(Analyte == analyte)
      } 
      
      measurement <- dataframe[1,'Measurement']
      proteinIDLabel <- trimws(str_split(rv$analytePlaceholder,':', simplify = TRUE)[2])
      
      dataframe <- dataframe %>%
        mutate(ProteinID = ifelse(grepl('Mass',input$Platform),UniProt,Aptamer)) %>%
        mutate(highlightGroup = case_when(RecordID %in% groupA ~ "A", RecordID %in% groupB ~ "B")) %>%   
        select(-c(GeneSymbol,Aptamer,UniProt,AgeGroup,Measurement,Kit_Barcode,ExperimentID)) %>%
        rename(`Specimen Type` = Specimen_type, `Seroconverion Group` = SeroconversionGroup,`Highlight Group` = highlightGroup) %>%
        select(RecordID,`Specimen Type`,Analyte,ProteinID,Description,Platform,Sex,GroupVariable,`Seroconverion Group`,`Highlight Group`,MeasuredValue) %>%
        rename(!!proteinIDLabel := ProteinID, !!analyteLabel := GroupVariable,!!rv$analyteLabel := Analyte, !!measurement := MeasuredValue)

      return(dataframe)

    }

    output$SelectedAnalyteRawDataTitle <- renderText({
      ifelse(input$Analyte=="",'All Sample Level Data',paste0('Sample Level Data for ', input$Analyte))
    })
   
    output$AnalyteDataTable <- DT::renderDataTable({
            
      dataframe <- GetAnalyteData(input$Platform, input$Sex, input$AgeGroup,input$Analyte, input$GroupA, input$GroupB) %>%
        select(-c(`Highlight Group`))

      DT::datatable(
        data = dataframe,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          paste0(input$Analyte, ' Data: '), htmltools::em(paste0(' Data for ',input$Analyte,' Box-Plot'))
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
        style = 'bootstrap'
      )
    }, server=TRUE)    

    # Box Plot ####
    output$AnalyteBoxPlot <- renderPlotly({
      
      dataset <- AnalyteDataset()
      
      if(nrow(dataset)>0) {  
        
        shinyjs::show("AnalyteContent")
        shinyjs::show("LogTransform")
        shinyjs::show("ExternalLinks")
        shinyjs::hide("AnalyteContentEmpty")
       
        dataset %>%      
          mutate(text = paste0(y)) %>%
          mutate(highlightGroup = case_when(RecordID %in% input$GroupA ~ "A", RecordID %in% input$GroupB ~ "B")) %>%
          select(key=RecordID,group=GroupVariable,value=y,valueLabel=y_label,text, highlightGroup) %>%
          CUSOMShinyHelpers::getBoxPlotWithHighlightGroup(key,group,baselineLabel,value,valueLabel,text,highlightGroup,plotName=paste0(id,"Analyte")) %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),legend=list(title=list(text=paste0("<b>",groupVariableLabel,"</b>"))))  %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "svg",
              filename = paste0(appConfig$applicationName, " - Analyte Box Plot ",format(Sys.time(),"%Y%m%d_%H%M%S")) ,
              width = session$clientData[[paste0('output_',id,'-AnalyteBoxPlot_width')]],
              height = session$clientData[[paste0('output_',id,'-AnalyteBoxPlot_height')]]
            ),
            modeBarButtons = list(
              list("select2d"), 
              list("lasso2d"),
              list("toImage"), 
              list(plotlyCustomIcons$BoxplotCompareGroup),
              list(plotlyCustomIcons$BoxplotClear)
            )
          ) %>% onRender("function(el) { overrideModebarDivId(el); }")
     
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
        paste0(
         'One of the groups chosen contains less than 10 samples. <br /> <br />
          Please re-adjust dataset options and try again.'
        )
      )
    })
        
    output$AnalyteBoxPlotPlotTitle <- renderUI({
    
      validate(
        need(!is.na(input$Analyte),""),
        need(input$Analyte != "","")
      )

      p <- shared_FoldChangeData$data(withSelection = FALSE) %>%
        filter(Analyte==input$Analyte) %>% 
        ungroup() %>%
        select(p.value,p.value.adjustment.method) 
     
      if(nrow(p) > 0) {
           
        HTML(
          paste0(
            '<h3>Effect of ',groupVariableLabel,' on ',input$Analyte,' in plasma
              <span onclick=\"launchTutorial(\'',id,'\',\'BoxPlot\')\"
                data-toggle="tooltip"
                data-placement="auto right" title="" class="fas fa-info-circle gtooltip"
                data-original-title="Use the box or lasso select to highlight records and see additional information below">
              </span>
            </h3>',
            CUSOMShinyHelpers::formatPValue(p$p.value,p$p.value.adjustment.method)
           
          )
        )
        
      } 
      
      else {
        
        HTML(
          paste0(
            '<h3>Please choose an analyte from the volcano plot
              <span onclick=\"launchTutorial(\'',id,'\',\'VolcanoPlot\')\"
                data-toggle="tooltip"
                data-placement="auto right" title="" class="fas fa-info-circle gtooltip"
                data-original-title="Use the box or lasso select to highlight records and see additional information below">
              </span>
            </h3>'
          )
        )
      }
      
    })

    output$BoxplotAnalyteEmptyTitle <- renderUI({
      HTML(
        paste0(
          '<h3>Unable to display plot ',ifelse(input$Analyte!="",paste0('for ',input$Analyte),''),
            '<span onclick=\"launchTutorial(\'',id,'\',\'DatasetOptions\')\" 
              data-toggle="tooltip"
              data-placement="auto right" title="" class="fas fa-info-circle gtooltip"
              data-original-title="Click here to learn about setting dataset options">
            </span>
          </h3>'
        )
      )
    })

    observeEvent(event_data("plotly_selected", source = paste0(id,"AnalyteBoxPlot") ), {
      
      e <- event_data("plotly_selected", source = paste0(id,"AnalyteBoxPlot") )
     
      recordIDs <- e %>% select(key) %>% pull()

      if(length(recordIDs) < 10) {
        
        rv$groupselectmodalstate <- 1
        
        showModal(
          modalDialog(
          title = "Cannot Highlight Group",
          HTML(
            paste0("You have selected only <b>",length(recordIDs), "</b> records and you must select at least <b><em>10</em></b> points to create a group. <br /><br />
                  Please try your selection again...")
               ),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = NS(id,"dismiss_groupselectmodal"),
                         label = "Dismiss")
            )
        )
      )

      } 

      else {

        # A is empty 
        if(is.null(input$GroupA)) {
          
          rv$lastGroupFilled <- "A"
          
          updatePickerInput(
            session = session,
            inputId = 'GroupA',
            selected = recordIDs
          )
          
        }
        
        #A is filled, B is empty 
        else if(!is.null(input$GroupA) & is.null(input$GroupB)) {
          
          rv$lastGroupFilled <- "B"
          
          updatePickerInput(
            session = session,
            inputId = 'GroupB',
            selected = recordIDs
          )
          
        }
        
        # both are filled, 
        ## check for overlap -- only add net new to the appropriate group
        else {
          
          if(rv$lastGroupFilled == "A") {
            
            # A was the last group filled, B is the target 
            # find the overlapping items in selected and A vectors
            recordIDs <- setdiff(recordIDs,input$GroupA)
            
            updatePickerInput(
              session = session,
              inputId = 'GroupB',
              selected = recordIDs
            )
            
            rv$lastGroupFilled <- "B"
          } 
          
          else {
            
            # B was the last group filled, A is the target 
            # find the overlapping items in selected and B vectors
            recordIDs <- setdiff(recordIDs,input$GroupB)
            
            updatePickerInput(
              session = session,
              inputId = 'GroupA',
              selected = recordIDs
            )
            
            rv$lastGroupFilled <- "A"
            
          }

        }

      }

    })

    observeEvent(c(input$dismiss_groupselectmodal),{
     
      #modal is open and the dismiss button has been clicked 
      if(rv$groupselectmodalstate == 1 & input$dismiss_groupselectmodal > 0) {
        
        # set state of modal back to closed 
        rv$groupselectmodalstate <- 0
        
        removeModal()
        
      }
     
    })
     
    observeEvent(c(input$GroupA,input$GroupB),{
      ### when 2 groups are filled in --- show the ability to compare groups
      
      if(length(input$GroupA) > 0 &  length(input$GroupB) > 0 ) {       
        
        if(input$GroupA != "" & input$GroupB != "") {
          
          updateSelectizeInput(
            session = session,
            inputId = "TutorialName",
            selected = "BoxplotGroupComparison"
          )
          
          updateSelectizeInput(
            session = session,
            inputId = "GroupAnalysisChoice", 
            selected = "To another analyte"
          )
        
          shinyjs::show("GroupAnalysisOptions")
        
        }
        
        else {
          
          shinyjs::hide("GroupAnalysisOptions")
          
        }

      }

      else {
        
        shinyjs::hide("GroupAnalysisOptions")
       
      }

    })

    observeEvent(c(input$GroupAnalysisChoice),{
      
      if(length(input$GroupAnalysisChoice) > 0 & input$GroupAnalysisChoice != "") {
        # toggle button
        shinyjs::enable("ShowGroupComparison")
      } 
      else {
        # disable button
        shinyjs::disable("ShowGroupComparison")
      }
    
      rv$groupComparisonChoice <- input$GroupAnalysisChoice
      
      if(input$GroupAnalysisChoice=="To another analyte") {
       
        AnalyteComparisionChoices <- shared_FoldChangeData$data(withSelection = FALSE) %>%
          filter(!Analyte %in% rv$selectedAnalyte$name ) %>%
          select(Analyte) %>%
          unique() %>%
          arrange() %>%
          pull()

        updateSelectizeInput(
          session = session, 
          inputId = "AnalyteComparision",
          choices = AnalyteComparisionChoices
        )

        shinyjs::hide("ComorbidityComparision")
        shinyjs::hide("RunComorbidityComparision")
        shinyjs::show("AnalyteComparision")

      } 

      else {
        shinyjs::hide("AnalyteComparision")
      }

      if(input$GroupAnalysisChoice=="Comorbidity Frequency") {
        shinyjs::hide("AnalyteComparision")
        shinyjs::show("ComorbidityComparision")
        shinyjs::show("RunComorbidityComparision")
      }

      else {
        shinyjs::hide("ComorbidityComparision")
      }
      
    })
    
    observeEvent(c(input$ComparisionIgnore),{
      
      validate(
        need(input$ComparisionIgnore[1]>0,'')
      )

      # hide the options, scroll back up. 
      shinyjs::hide("GroupAnalysisOptions")   
      shinyjs::runjs(paste0('document.getElementById("',id,'-AnalyteBoxPlot").scrollIntoView(); '))
       
    })

    ComparisonDatasets <- reactive({

      validate(
        need(rv$groupComparisonChoice == input$GroupAnalysisChoice,''),    
        need(input$GroupAnalysisChoice != "",''), 
        need(input$Analyte!='','')
      )
   
      analyteDataset <- getAnalyteDataset(input$Platform,input$Sex,input$AgeGroup,input$Analyte,input$LogTransform,input$GroupA,input$GroupB) %>%    
        mutate(text = paste0(RecordID,'<br />',MeasuredValue)) %>%
        filter(!is.na(highlightGroup))
      
      comparisonAnalyteDataset <- getAnalyteDataset(input$Platform,input$Sex,input$AgeGroup,input$AnalyteComparision,input$LogTransform,input$GroupA,input$GroupB) %>%
        mutate(text = paste0(RecordID,'<br />',MeasuredValue)) %>%
        filter(!is.na(highlightGroup)) 
            
      return(
        list(
        "analyteDataset" =  analyteDataset, 
        "comparisonAnalyteDataset" = comparisonAnalyteDataset
        )
      )
    
    })
      
    output$AnalyteGroupComparisonPlot <- renderPlotly({
      
      validate(
        need(!is.na(input$GroupAnalysisChoice),''),
        need(rv$groupComparisonChoice == input$GroupAnalysisChoice,'')
      )
      
      comparisonDatasets <- ComparisonDatasets()
      
      if(grepl('Sex',input$GroupAnalysisChoice)) {
        
        p <- comparisonDatasets$sexData %>%
          getSelectedRecordsSexPlot(highlightGroup) %>%
          layout(yaxis = list(title="Highlighted Group",fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE)) %>%
          layout(title = "Comparison of Sex Distriubtion")

      }
      
      if(grepl('Age',input$GroupAnalysisChoice)) {

        p <- comparisonDatasets$ageData %>%
          getSelectedRecordsAgePlot(highlightGroup,AgeAtTimeOfVisit) %>%
          layout(xaxis = list(title="Highlighted Group",fixedrange = TRUE)) %>%
          layout(yaxis = list(title="Age",fixedrange = TRUE)) %>%
          layout(title = "Comparison of Age Distriubtion")
          
      }
      
      if(grepl('analyte',input$GroupAnalysisChoice)) {
        
        if(nrow(comparisonDatasets$analyteDataset) > 0 & nrow(comparisonDatasets$comparisonAnalyteDataset) > 0 ) {

          analyteLabel <- comparisonDatasets$analyteDataset %>% select(Analyte) %>% unique() %>% pull()
          
          pval1 <- comparisonDatasets$analyteDataset %>%
            mutate(log2MeasuredValue = ifelse(MeasuredValue==0,0,log2(MeasuredValue))) %>%
            mutate(GroupVariable = highlightGroup) %>%
            mutate(GroupVariable = fct_relevel(GroupVariable, "A")) %>% # set ref level
            select(RecordID, Analyte, log2MeasuredValue, GroupVariable, Sex, AgeGroup) %>%
            CUSOMShinyHelpers::getStatTestByKeyGroup(RecordID, Analyte, GroupVariable, "A", log2MeasuredValue, method = input$StatTest, 
                                                     adjustmentMethod = input$AdjustmentMethod, GroupVariable, Sex, AgeGroup) %>%
            select(p.value) %>%
            pull()
          
          pval1text <- paste0('<b>',CUSOMShinyHelpers::formatPValue(pval1,input$AdjustmentMethod),'</b>')
          
          
          comparisonLabel <- comparisonDatasets$comparisonAnalyteDataset %>% select(Analyte) %>% unique() %>% pull()
          
          pval2 <- comparisonDatasets$comparisonAnalyteDataset %>%
            mutate(log2MeasuredValue = ifelse(MeasuredValue==0,0,log2(MeasuredValue))) %>%
            mutate(GroupVariable = highlightGroup) %>%
            mutate(GroupVariable = fct_relevel(GroupVariable, "A")) %>% # set ref level
            select(RecordID, Analyte, log2MeasuredValue, GroupVariable, Sex, AgeGroup) %>%
            CUSOMShinyHelpers::getStatTestByKeyGroup(RecordID, Analyte, GroupVariable, "A", log2MeasuredValue, method = input$StatTest, 
                                                     adjustmentMethod = input$AdjustmentMethod, GroupVariable, Sex, AgeGroup) %>%
            select(p.value) %>%
            pull()
          
          pval2text <- paste0('<b>',CUSOMShinyHelpers::formatPValue(pval2,input$AdjustmentMethod),'</b>')
          
          combinedData <- comparisonDatasets$analyteDataset %>%
          bind_rows(comparisonDatasets$comparisonAnalyteDataset)
        
          p <- combinedData %>%
            CUSOMShinyHelpers::getSideBySideGroupedBoxplot(RecordID,highlightGroup,Analyte,analyteLabel,y,y_label,text,TRUE,"AnalyteComparison") %>%
            layout(
              title = paste0("Comparison Between ",analyteLabel," and ",comparisonLabel,""),
              annotations = list(
                list(
                  x = 0.225, 
                  y = 1.05, 
                  font = list(size = 16), 
                  text = pval1text,
                  xref = "paper", 
                  yref = "paper", 
                  xanchor = "center", 
                  yanchor = "bottom", 
                  showarrow = FALSE
                ), 
                list(
                  x = 0.775, 
                  y = 1.05, 
                  font = list(size = 16), 
                  text = pval2text,
                  xref = "paper", 
                  yref = "paper", 
                  xanchor = "center", 
                  yanchor = "bottom", 
                  showarrow = FALSE
                )
              )
            )
      
        }
        else {
          p <- NULL
        }

      }

      if(grepl('Comorb',input$GroupAnalysisChoice)) {
        
        validate(
          need(!is.na(input$ComorbidityComparision),'')
        )
        
        pval1 <- comparisonDatasets$comorbidityDataset %>%
          filter(HasAnyConditionFlag=="Yes") %>%
          getStatTestByKeyGroup(RecordID,HasAnyConditionFlag,highlightGroup,y,input$StatTest,input$AdjustmentMethod) %>%
          select(p.value) %>%
          pull()
        
        pval1text <- paste0('<b>',CUSOMShinyHelpers::formatPValue(pval1,input$AdjustmentMethod),'</b>')
        
        pval2 <- comparisonDatasets$comorbidityDataset %>%
          filter(HasAnyConditionFlag=="No") %>%
          getStatTestByKeyGroup(RecordID,HasAnyConditionFlag,highlightGroup,y,input$StatTest,input$AdjustmentMethod) %>%
          select(p.value) %>%
          pull()
        
        pval2text <- paste0('<b>',CUSOMShinyHelpers::formatPValue(pval2,input$AdjustmentMethod),'</b>')
        
        p <- comparisonDatasets$comorbidityDataset %>%
          mutate(text = paste0(RecordID,'<br />', y, '<br />',Conditions)) %>%
          mutate(HasAnyConditionFlag = case_when(HasAnyConditionFlag=="No"~"Does not have any selected conditions",
                                                  HasAnyConditionFlag=="Yes"~"Has at least 1 selected condition")) %>%
          drop_na() %>%
          CUSOMShinyHelpers::getSideBySideGroupedBoxplot(RecordID,highlightGroup,HasAnyConditionFlag,"Has at least 1 selected condition",y,y_label,text,TRUE,"AnalyteComparison") %>%
          layout(
            title = paste0("Comorbidity Frequency Comparison Between Groups"),
            annotations = list(
              list(
                x = 0.225, 
                y = 1.05, 
                font = list(size = 16), 
                text = pval1text,
                xref = "paper", 
                yref = "paper", 
                xanchor = "center", 
                yanchor = "bottom", 
                showarrow = FALSE
              ), 
              list(
                x = 0.775, 
                y = 1.05, 
                font = list(size = 16), 
                text = pval2text,
                xref = "paper", 
                yref = "paper", 
                xanchor = "center", 
                yanchor = "bottom", 
                showarrow = FALSE
              )
            )
          ) 
      }
      
      if(!is.null(p)) {
        
        p %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            modeBarButtons = list(
              list("toImage"), 
              list(plotlyCustomIcons$BoxplotClear)
            )
          ) %>% onRender("function(el) { overrideModebarDivId(el); }")
        
      } 
      
      else {
        
        getBoxplotForEmptyData(text = "")
        
      }
      
    }) 
    
    output$SelectedAnalyteHighlightGroupsDataTitle <- renderUI ({
      paste0(input$Analyte,' Sample Level Data Highlighted Groups')
    })

    output$SelectedAnalyteRecordsDataTable <- DT::renderDataTable({
      
      dataframe <- GetAnalyteData(input$Platform, input$Sex, input$AgeGroup,input$Analyte, input$GroupA, input$GroupB) %>%
        filter(!is.na(`Highlight Group`)) 
      
      DT::datatable(
        data = dataframe ,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          paste0(input$Analyte,' Sample Level Data: '), htmltools::em(paste0('Sample Level Data for highlighted records on ',input$Analyte,' Box-Plot'))
        ),
        filter = 'top',
        extensions = c('Buttons','ColReorder','Responsive','Scroller'),
        options = list(
          dom = 'Bfrtip',
          colReorder = TRUE,
          autowidth=FALSE,
          deferRender = TRUE,
          scrollX =TRUE,
          scrollY = 400,
          scroller = TRUE,
          columnDefs = list(list(width = '200px', targets = "_all")),
          pageLength = 10,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')
        ),
        rownames = FALSE,
        style = 'bootstrap'
      )

    }, server=FALSE)

    
  })
  
  
}