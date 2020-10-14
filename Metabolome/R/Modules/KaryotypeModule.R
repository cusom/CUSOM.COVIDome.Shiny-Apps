KaryotypeUI <- function(id) {
  list(
    "Inputs" = 
      list(
        div(
          id="Dataset-Options",class="sidebar-text",
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
        ,tags$hr()
        ,CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"StatTest"),label = "Statistical Test", choices = statTests ,selected = statTests[1])
        ,CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"AdjustmentMethod"),label = "Adjustment Method", choices = adjustmentMethods, selected = "none")
        ,CUSOMShinyHelpers::createInputControl(controlType = "checkboxGroupInput", inputId = NS(id,"Sex"),label = "Sex", choices = sexes ,selected = sexes, inline=TRUE )
        ,CUSOMShinyHelpers::createInputControl(controlType = "checkboxGroupInput", inputId = NS(id,"AgeGroup"),label = "Age Group", choices = ageGroups ,selected = ageGroups, inline=TRUE )
        ,selectizeInput(
          NS(id,"Analyte"),
          label="Analyte",
          choices= analytes,
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
        ,tags$br()
        ,actionButton(NS(id,"VolcanoDatasetRefresh"), "Apply filters and generate plot", class = "refresh-btn")
        ,tags$hr()
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
                    withSpinner(plotlyOutput(NS(id,"VolcanoPlot"),height = "650px"))
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
                        CUSOMShinyHelpers::createInputControl(controlType = "primarySwitch", inputId = NS(id,"LogTransform"),label = HTML("Show as Log<sub>2</sub> Transformed?"), status="primary", value = TRUE)
                        ),
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"GroupA"),label = "Group A", choices = recordIDs, selected = NULL, multiple = TRUE )
                        ),
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"GroupB"),label = "Group B", choices = recordIDs, selected = NULL, multiple = TRUE )
                        ),                  
                      withSpinner(plotlyOutput(NS(id,"AnalyteBoxPlot"),height = "630px"))                   
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
                      height = 120,
                      sliderInput(
                        inputId = NS(id,"FoldChange"),
                        label = HTML("Filter by Log<sub>2</sub> Fold Change"),
                        min = 0,
                        max = 10,
                        step = 0.05,
                        value = c(0,10)
                      )
                    ), 
                    box(
                      width = 4,
                      height = 120,
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
                    DT::dataTableOutput(NS(id,"FoldChangeDataTable"))
                  )
                )
              )
            )
          )
        ),
        tags$hr(),
        fluidRow(
          HTML(appConfig$footerHTML)
        )
    )
  ) 
}


KaryotypeServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {

     ### Reactive Values #
    rv <- reactiveValues(RunRefresh = -1, 
                         Platform = "",
                         maxFoldChange = 0,
                         lastGroupFilled = NA ,
                         selectedAnalyte = list(
                           pvalue = -1, 
                           xCoordiante = NA, 
                           yCoordinate = NA, 
                           name = ''
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
      
      if(input$TutorialName != '') {
        
        introjs(session = session, options=list(steps=tutorialSteps()))
        
        updateSelectizeInput(
          session = session, 
          inputId = "TutorialName", 
          selected = ""
        )    
        
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
      shinyjs::show("AnalyteContent")
      shinyjs::hide("AnalyteContentEmpty")
      
    })
      
    # change plots tab title based on chosen platform
    output$PlotsTitle <- renderUI({
      paste0('Plots')
    })
        
    # BASE DATASET WITH UI FILTERS APPLIED 
    dataWithFilters <- eventReactive(c(input$VolcanoDatasetRefresh),{
     
      dataframe <- sourceData %>%
        filter(Sex %in% input$Sex) %>%
        filter(AgeGroup %in% input$AgeGroup) %>%
        CUSOMShinyHelpers::applyGroupCountThreshold(Status,Analyte, threshold = 1 ) 
      
      if(nrow(dataframe) > 0) {
        return(dataframe)
      } 
      else {
        return(NULL)
      }
        
    }, ignoreInit = TRUE)
    
    shared_dataWithFilters <- SharedData$new(dataWithFilters)

    FoldChangeData <- eventReactive(c(input$VolcanoDatasetRefresh),{
      
      baseData <- dataWithFilters()

      if(!is.null(baseData)) { 

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
            
        foldChange <- baseData %>%
          CUSOMShinyHelpers::summarizeByGroup(MeasuredValue, Analyte, Status, na.rm = TRUE) %>%
          CUSOMShinyHelpers::calculateFoldChangeByKeyGroup(Analyte, Status, median, "Negative")
        
        update_modal_progress(
          value = 2,
          text = paste0("Running ",names(which(statTests == input$StatTest)),"..."),
          session = shiny::getDefaultReactiveDomain()
        )
        
        statsData <- baseData %>%
          mutate(log2MeasuredValue = log2(MeasuredValue)) %>%
          CUSOMShinyHelpers::getStatTestByKeyGroup(RecordID,Analyte,Status,log2MeasuredValue,input$StatTest, input$AdjustmentMethod)
        
        update_modal_progress(
          value = 3,
          text = ifelse(input$AdjustmentMethod=="none",paste0("Running ",names(which(statTests == input$StatTest)),"..."),paste0("Adjusting P Values using ",input$AdjustmentMethod," method...")),
          session = shiny::getDefaultReactiveDomain()
        )

        finalData <- inner_join(foldChange, statsData, by="Analyte") %>%
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
        
        return(finalData)

      }

      else {

        return(NULL)

      }  

    })

    # change the fold change tab title based on platform
    output$FoldChangeDataTitle <- renderUI ({
      paste0('Fold Change Raw Data')
    })
    
    shared_FoldChangeData <- SharedData$new(FoldChangeData)
    
    FoldChangeDataTableData <- reactive({
      
      dataframe <- shared_FoldChangeData$data(withSelection = FALSE) %>%
        mutate(pvalueCutoff = case_when(input$PValue=="all" ~ 1 , input$PValue==" * P &le; 0.05" ~ 0.05, input$PValue==" ** P &le; 0.01" ~ 0.01 , input$PValue==" *** P &le; 0.001" ~ 0.001)) %>%
        filter(log2Foldchange >= min(input$FoldChange), log2Foldchange <= max(input$FoldChange)) %>%
        filter(p.value <= pvalueCutoff) %>%
        select(Analyte,FoldChange,p.value,p.value.original,`Positive`, `Negative`,log2Foldchange,`-log10pvalue`,method,p.value.adjustment.method)
      
      if(nrow(dataframe) > 0) {
        
        if(unique(dataframe$p.value.adjustment.method=="none")) {
          
          dataframe <- dataframe %>%
            select(-c(p.value.original,p.value.adjustment.method)) %>%
            rename( "Fold Change (Positive/Negative)" = FoldChange , 
                    "p-value" = p.value, 
                    "Positive Median" = `Positive`, 
                    "Negative Median" = `Negative`, 
                    "log<sub>2</sub> Fold Change" = `log2Foldchange` , 
                    "-log<sub>10</sub> p-value" = `-log10pvalue` , 
                    "Statistical test" = method
            )
          
        }
        
        else {
          
          dataframe <- dataframe %>%
            rename( "Fold Change (Positive/Negative)" = FoldChange , 
                    "p-value (adj)" = p.value, 
                    "p-value (original)" = p.value.original,
                    "Positive Median" = `Positive`, 
                    "Negative Median" = `Negative`, 
                    "log<sub>2</sub> Fold Change" = `log2Foldchange` , 
                    "-log<sub>10</sub> p-value (adj)" = `-log10pvalue` , 
                    "Statistical test" = method, 
                    "Adjustment Method" = p.value.adjustment.method
            )
          
        }
        
        return(dataframe)
      
      } 
      
      else {
        
        return(NULL)

      }
      
    })
        
    output$FoldChangeDataTable <- DT::renderDataTable({
    
      DT::datatable(
        data=FoldChangeDataTableData(),
        caption = htmltools::tags$caption(
           style = 'caption-side: bottom; text-align: center;',
            'Fold Change Data: ', htmltools::em('Raw Fold Change Data Used for Volcano Plot')
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
     
      dataframe <- FoldChangeData()
      
      if(!is.null(dataframe)) {
     
        p.value.suffix <- ifelse(unique(dataframe$p.value.adjustment.method)=="none","","(adj) ")
        yaxis.title <- paste0("p-value ",p.value.suffix,"(-log<sub>10</sub>)")

        a <- dataframe %>% CUSOMShinyHelpers::getVolcanoAnnotations(log2Foldchange,`-log10pvalue`, `selected_`, Analyte, pValueThreshold,'Up in COVID-19 +')

        shinyjs::show("VolcanoContent")
        shinyjs::hide("VolcanoContentEmpty")
       
        dataframe %>%
          mutate(text = paste0("Metabolite:", Analyte,
                               "<br />fold_change:", round(FoldChange,2),
                               "<br />p-value",p.value.suffix,": ",formatC(p.value, format = "e", digits = 2) 
                                )
                  ) %>% 
          CUSOMShinyHelpers::AddSignificanceGroup(log2Foldchange,`-log10pvalue`, pValueThreshold) %>%
          CUSOMShinyHelpers::getVolcanoPlot(log2Foldchange,`-log10pvalue`, significanceGroup, text, Analyte, "CovidStatus") %>%
          layout(xaxis = list(title="Fold Change (log<sub>2</sub>)",fixedrange = FALSE)) %>%
          layout(yaxis = list(title=yaxis.title,fixedrange = FALSE)) %>%
          layout(annotations=a) %>%
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
        CUSOMShinyHelpers::getBoxplotForEmptyData(text = rv$errorMessage)

      }

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
     
      title <- ifelse(input$VolcanoDatasetRefresh,paste0('Effect of COVID-19 status on all metabolites in plasma'),'Please start by setting dataset options below')
      
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
    observeEvent(event_data("plotly_click", source = "CovidStatusVolcanoPlot"),{
      
      e <- event_data("plotly_click", source = "CovidStatusVolcanoPlot")
      
      updateSelectizeInput(
        session = session,
        inputId = "Analyte",
        selected = e$key
      )
      
    })
    
    observeEvent(c(input$Analyte),{
   
      if (input$Analyte != '') {
        
        r <- shared_FoldChangeData$data(withSelection = FALSE) %>%
          filter(Analyte==input$Analyte) %>%
          as_tibble() %>%
          select(p.value, name = Analyte, x = log2Foldchange, y = `-log10pvalue`) %>%
          as.list()
        
        rv$selectedAnalyte$pvalue <- as.numeric(r$p.value)
        rv$selectedAnalyte$xCoordiante <- as.numeric(r$x)
        rv$selectedAnalyte$yCoordinate <- as.numeric(r$y)
        rv$selectedAnalyte$name <- r$name

        a <- shared_FoldChangeData$data(withSelection = FALSE) %>%
          mutate(selected_ = case_when(Analyte==input$Analyte ~ 1)) %>%
          CUSOMShinyHelpers::getVolcanoAnnotations(log2Foldchange,`-log10pvalue`, `selected_`, Analyte, pValueThreshold,'Up in COVID-19 +') 
        
        plotlyProxy("VolcanoPlot", session) %>%
          plotlyProxyInvoke("relayout", list(annotations = a))
      
      }
      
    })

    # # Reactive Data #### 
    AnalyteDataset <- eventReactive(c(input$VolcanoDatasetRefresh,input$Analyte, input$LogTransform), {
      
      validate(
        need(!is.na(input$Analyte),""),
        need(input$Analyte != "",""),    
        need(input$VolcanoDatasetRefresh[1]>0,"")

      )
      
      dataframe <- dataWithFilters()
     
      if(!is.null(dataframe)) {
       
        dataframe %>%
          filter(Analyte==input$Analyte) %>%
          mutate(y = case_when(input$LogTransform==TRUE ~ log2(MeasuredValue), input$LogTransform==FALSE ~ MeasuredValue), 
                y_label = case_when(input$LogTransform==TRUE ~ paste0("Log<sub>2</sub> ", Measurement), input$LogTransform==FALSE ~ Measurement )
          ) %>% CUSOMShinyHelpers::applyGroupCountThreshold(Status, threshold = 10)        
      }

      else {
        NULL
      }

    })
        
    shared_AnalyteDataset <- SharedData$new(AnalyteDataset)
        
    # Karyotype Box Plot ####
    output$AnalyteBoxPlot <- renderPlotly({
      
      dataset <- AnalyteDataset()
      
      if(!is.null(dataset)) {
        
        shinyjs::show("AnalyteContent")
        shinyjs::show("LogTransform")
        shinyjs::hide("AnalyteContentEmpty")

        dataset %>%      
          mutate(text = paste0(RecordID,'<br />',y)) %>%
          mutate(HighlightGroup = case_when(RecordID %in% input$GroupA ~ "A", RecordID %in% input$GroupB ~ "B")) %>%
          CUSOMShinyHelpers::getBoxPlotWithHighlightGroup(RecordID,Status,"Negative",y,y_label,text,HighlightGroup,"Analyte") %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
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
      
      if(!is.na(p$p.value) ) {
           
        HTML(
          paste0(
            '<h3>Effect of COVID-19 status on ',input$Analyte,' in plasma
              <span onclick=\"launchTutorial(\'',id,'\',\'BoxPlot\')\"
                data-toggle="tooltip"
                data-placement="auto right" title="" class="fas fa-info-circle gtooltip"
                data-original-title="Use the box or lasso select to highlight records and see additional information below">
              </span>
            </h3>',
            CUSOMShinyHelpers::formatPValue(p$p.value,p$p.value.adjustment.method,pValueThreshold)          
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
          '<h3>Unable to display plot for ',input$Analyte,'
            <span onclick=\"launchTutorial(\'',id,'\',\'DatasetOptions\')\" 
              data-toggle="tooltip"
              data-placement="auto right" title="" class="fas fa-info-circle gtooltip"
              data-original-title="Click here to learn about setting dataset options">
            </span>
          </h3>'
        )
      )
    })
    
    observeEvent(event_data("plotly_selected", source = "AnalyteBoxplot") ,{

      e <- event_data("plotly_selected", source = "AnalyteBoxplot")
     
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

    
    AnalyteComparisonDataset <- reactive({  

      validate(
        need(input$AnalyteComparision!='','')
      )
   
      shared_dataWithFilters$data(withSelection = FALSE) %>%
        filter(Analyte==input$AnalyteComparision) %>%
        mutate(y = case_when(input$LogTransform==TRUE ~ log2(MeasuredValue), input$LogTransform==FALSE ~ MeasuredValue), 
               y_label = case_when(input$LogTransform==TRUE ~ paste0("Log<sub>2</sub> ", Measurement), input$LogTransform==FALSE ~  Measurement )) %>%
        mutate(text = paste0(RecordID,'<br />',MeasuredValue)) %>%
        mutate(HighlightGroup = case_when(RecordID %in% input$GroupA ~ "A", RecordID %in% input$GroupB ~ "B")) %>%
        filter(!is.na(HighlightGroup))
      
    })
    
       
    HighlightGroupComparisonDataset <- reactive({ 
      
      validate(
        need(rv$groupComparisonChoice == input$GroupAnalysisChoice,''), 
        need(input$GroupAnalysisChoice != "",'')
      )
      
      dataframe <- shared_AnalyteDataset$data(withSelection = FALSE) %>%      
        mutate(y = case_when(input$LogTransform==TRUE ~ log2(MeasuredValue), input$LogTransform==FALSE ~ MeasuredValue), 
               y_label = case_when(input$LogTransform==TRUE ~ paste0("Log<sub>2</sub> ", Measurement), input$LogTransform==FALSE ~  Measurement )) %>%
        mutate(text = paste0(RecordID,'<br />',MeasuredValue)) %>%
        mutate(HighlightGroup = case_when(RecordID %in% input$GroupA ~ "A", RecordID %in% input$GroupB ~ "B")) %>%
        filter(!is.na(HighlightGroup))
      
      
      if(grepl('Sex',input$GroupAnalysisChoice)) {
        
        return(
          dataframe %>%
            group_by(HighlightGroup,Gender) %>%
            summarise(n=n_distinct(LabID)) %>%
            inner_join(
              dataframe  %>%
                group_by(HighlightGroup) %>%
                summarise(All=n_distinct(record_id))
              , on=c("HighlightGroup")
            ) %>%
            mutate(Percent = n / All) %>%
            select(HighlightGroup,Gender,Percent) %>%
            fillMissingGenderObservations(HighlightGroup,c("A","B")) %>%
            spread(Gender,Percent) 
        )

      }

      else if(grepl('Age',input$GroupAnalysisChoice)) {
        
        return(
          dataframe %>%
            select(HighlightGroup,AgeAtTimeOfVisit)
          )

      }

      else if(grepl('analyte',input$GroupAnalysisChoice)) {        
        
      
        dataframeB <- AnalyteComparisonDataset()
       
        return(rbind(dataframeB ,dataframe))
        
      }

      else if(grepl('Comorb',input$GroupAnalysisChoice)) {
        
        return(
          
          ParticipantConditions %>%
            filter(Condition %in% input$ComorbidityComparision ) %>%
            mutate(HasConditionFlag = case_when(HasCondition=='True'~1, HasCondition=='False'~0)) %>%
            select(LabID,Condition, HasCondition, HasConditionFlag) %>%
            group_by(LabID) %>%
            summarise(HasAnyConditionFlag = sum(HasConditionFlag)) %>%
            mutate(HasAnyConditionFlag = ifelse(HasAnyConditionFlag>0,1,0)) %>%
            drop_na() %>% 
            right_join(dataframe,conditionData,by="LabID") %>%
            select(LabID,HasAnyConditionFlag, HighlightGroup,y,y_label)
        )

      } 
      
      else {
        return(NULL)
      }

    })
      
    output$AnalyteGroupComparisonPlot <- renderPlotly({
      
      validate(
        need(rv$groupComparisonChoice == input$GroupAnalysisChoice,'')
      )

      dataframe <- HighlightGroupComparisonDataset()
      
      if(grepl('analyte',input$GroupAnalysisChoice)) {
       
        pval1 <- dataframe %>%
          filter(Analyte==input$Analyte) %>%
          getStatTestByKeyGroup(RecordID,Analyte,HighlightGroup,y,input$StatTest,input$AdjustmentMethod) %>%
          select(p.value) %>%
          pull()
        
        pval1text <- paste0('<b>',CUSOMShinyHelpers::formatPValue(pval1,input$AdjustmentMethod,pValueThreshold),'</b>')
        
        pval2 <- dataframe %>%
          filter(Analyte==input$AnalyteComparision) %>%
          getStatTestByKeyGroup(RecordID,Analyte,HighlightGroup,y,input$StatTest,input$AdjustmentMethod) %>%
          select(p.value) %>%
          pull()
       
        pval2text <- paste0('<b>',CUSOMShinyHelpers::formatPValue(pval2,input$AdjustmentMethod,pValueThreshold),'</b>')

        p <- dataframe %>%
          CUSOMShinyHelpers::getSideBySideGroupedBoxplot(RecordID,HighlightGroup,Analyte,input$Analyte,y,y_label,text,TRUE,"AnalyteComparison") %>%
          layout(
            title = paste0("Comparison Between ",input$Analyte," and ",input$AnalyteComparision,""),
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
            toImageButtonOptions = list(
              format = "svg",
              filename = paste0(appConfig$applicationName, " - Analyte Group Comparison ",format(Sys.time(),"%Y%m%d_%H%M%S")) ,
              width = session$clientData[[paste0('output_',id,'-AnalyteGroupComparisonPlot_width')]],
              height = session$clientData[[paste0('output_',id,'-AnalyteGroupComparisonPlot_height')]]
            ),
            modeBarButtons = list(
              list("toImage")
              #list(plotlyCustomIcons$BoxplotClear)
            )
          ) %>% onRender("function(el) { overrideModebarDivId(el); }")
        
      } 
      
      else {
        
        getBoxplotForEmptyData(text = "")
        
      }
      
    })
    
  })
  
  
}