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
                        CUSOMShinyHelpers::createInputControl(controlType = "primarySwitch", inputId = NS(id,"LogTransform"),label = HTML("Show as Log<sub>2</sub> Transformed?"), status="primary")
                        ),
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"GroupA"),label = "Group A", choices = NULL, selected = NULL, multiple = TRUE )
                        ),
                      shinyjs::hidden(
                        CUSOMShinyHelpers::createInputControl(controlType = "pickerInput", inputId = NS(id,"GroupB"),label = "Group B", choices = NULL, selected = NULL, multiple = TRUE )
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
                        label = "Filter by Fold Change",
                        min = 0,
                        max = 6,
                        step = 0.05,
                        value = c(0,6)
                      )
                    ), 
                    box(
                      width = 3,
                      height = 120,
                      awesomeRadio(
                        inputId = NS(id,"PValue"),
                        label = "Filter by p-value significance level", 
                        choices = c('all',"*", "**", "***",'not significant'),
                        selected = "all",
                        inline = TRUE
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
          ),
          tabPanel(title = uiOutput(NS(id,"SelectedAnalyteRawDataTitle")),
            fluidRow(
              box(
                title = "",
                id = NS(id,"tabpanel3"),
                height="auto",
                width = 11,
                DT::dataTableOutput(NS(id,"AnalyteDataTable"))
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
                         errorMessage = ""
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

    FoldChangeData <- eventReactive(input$VolcanoDatasetRefresh,{
      
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
          CUSOMShinyHelpers::SummarizeByGroup(MeasuredValue, Analyte, Status) %>%
          CUSOMShinyHelpers::calculateFoldChangeByKeyGroup(Analyte, Status, median, "Negative")
        
        update_modal_progress(
            value = 2,
            text = "Calculating P Values...",
            session = shiny::getDefaultReactiveDomain()
          )
        
        statsData <- baseData %>%
          CUSOMShinyHelpers::getStatTestByKeyGroup(RecordID,Analyte,Status,MeasuredValue,'t.test')
        
        update_modal_progress(
            value = 3,
            text = "Finalizing plot...",
            session = shiny::getDefaultReactiveDomain()
          )
          
        finalData <- inner_join(foldChange, statsData, by="Analyte") %>%
          mutate(selected_ = ifelse(Analyte==input$Analyte,1,0))
        
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
      
      shared_FoldChangeData$data(withSelection = FALSE) %>%
        mutate(pvalsignificance = case_when(input$PValue=="all" ~"all", p.value <= 0.001 ~ "***", p.value <= 0.01 ~ "**", p.value <= 0.05 ~ "*", p.value > 0.05 ~ "not significant")) %>%
        filter(FoldChange >= min(input$FoldChange), FoldChange <= max(input$FoldChange)) %>%
        filter(pvalsignificance == input$PValue) %>%
        select(Analyte,FoldChange,p.value,`Positive`, `Negative`,log2Foldchange,`-log10pvalue`) %>%
        rename( "Fold Change (Positive/Negative)" = FoldChange , 
                "p-value" = p.value, 
                "Postive Median" = `Positive`, 
                "Negative Median" = `Negative`, 
                "log<sub>2</sub> Fold Change" = `log2Foldchange` , 
                "-log<sub>10</sub> p-value" = `-log10pvalue`
        )
      
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
     
        a <- dataframe %>% CUSOMShinyHelpers::getVolcanoAnnotations(log2Foldchange,`-log10pvalue`, `selected_`, Analyte, pValueThreshold,'Up in positives') 
        shinyjs::show("VolcanoContent")
        shinyjs::hide("VolcanoContentEmpty")
       
        dataframe %>%
          mutate(text = paste0("Metabolite:", Analyte,
                                '<br />fold_change:', round(FoldChange,2),
                                '<br />p-value: ',formatC(p.value, format = "e", digits = 2) 
                                )
                  ) %>% 
          CUSOMShinyHelpers::AddSignificanceGroup(log2Foldchange,`-log10pvalue`, pValueThreshold) %>%
          CUSOMShinyHelpers::getVolcanoPlot(log2Foldchange,`-log10pvalue`, significanceGroup, text, Analyte, "CovidStatus") %>%
          layout(xaxis = list(title="Fold Change (log<sub>2</sub>)",fixedrange = TRUE)) %>%
          layout(yaxis = list(title="p value (-log<sub>10</sub>)",fixedrange = TRUE)) %>%
          layout(annotations=a) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            modeBarButtons = list(
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
     
      title <- ifelse(input$VolcanoDatasetRefresh,paste0('Effect of Covid 19 Status on all metabolites in Plasma'),'Please start by setting dataset options below')
      
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
          CUSOMShinyHelpers::getVolcanoAnnotations(log2Foldchange,`-log10pvalue`, `selected_`, Analyte, pValueThreshold,'Up in positives') 
        
        plotlyProxy("VolcanoPlot", session) %>%
          plotlyProxyInvoke("relayout", list(annotations = a))
      
      }
      
      else {
        
        ### DIM ALL TRACES IN CURRENT VOLCANO 
        plotlyProxy("VolcanoPlot", session) %>%
          plotlyProxyInvoke(
            method = "restyle",
            opacity = 0.0
          )
        
        #Volcano should have 3 traces, remove each. 
        plotlyProxy("VolcanoPlot", session) %>%
          plotlyProxyInvoke(
            "deleteTraces", 
            list(as.integer(0),as.integer(1),as.integer(2))
            ) 
       
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

    output$SelectedAnalyteRawDataTitle <- renderText({
      paste0(input$Analyte,' Raw Data')
    })
    
    output$AnalyteDataTable <- DT::renderDataTable({
      DT::datatable(
        data= shared_AnalyteDataset$data(withSelection = FALSE),
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          'Selected Analyte Raw Data: ', htmltools::em('Raw Data Used for Analyte Box-Plot')
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
    }, server=FALSE)
    
    
    # Karyotype Box Plot ####
    output$AnalyteBoxPlot <- renderPlotly({
      
      dataset <- AnalyteDataset()
      
      if(!is.null(dataset)) {
        
        shinyjs::show("AnalyteContent")
        shinyjs::show("LogTransform")
        shinyjs::hide("AnalyteContentEmpty")

        dataset %>%      
          mutate(text = paste0(RecordID,'<br />',y)) %>%
          #mutate(text = case_when(grepl('Meso',input$Platform) ~ paste0(RecordID,'<br />',y), grepl('SOMA',input$Platform) ~ paste0(RecordID,'<br />',y))) %>%
          mutate(HighlightGroup = case_when(RecordID %in% input$GroupA ~ "A", RecordID %in% input$GroupB ~ "B")) %>%
          CUSOMShinyHelpers::getBoxPlotWithHighlightGroup(RecordID,Status,"Negative",y,y_label,text,HighlightGroup,"AnalyteBoxPlot") %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            modeBarButtons = list(
              list("toImage")
              # ,list(plotlyCustomIcons$BoxplotCompareGroup)
              # ,list(plotlyCustomIcons$BoxplotClear) 
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
 
      pval <- rv$selectedAnalyte$pvalue
      
      if(!is.na(pval) ) {
           
        HTML(
          paste0(
            '<h3>Effect of Covid 19 Status on ',input$Analyte,' in plasma</h3>',
            CUSOMShinyHelpers::formatPValue(pval,pValueThreshold)
           
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
    
    
  })
  
  
}