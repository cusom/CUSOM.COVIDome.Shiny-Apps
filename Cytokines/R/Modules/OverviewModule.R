

OverviewUI <- function(id) {
  list(
    "Inputs" = 
      list(
        #materialSwitch(inputId = NS(id,"AppTutorial"),label = "Launch Tutorial", value = TRUE,status = "primary")
        ), 
    "Outputs" = 
      list(
        fluidRow(
          column(
            10,
            box(
              title ="",
              id = NS(id,"Overview"),
              height= "auto",
              width = "auto", 
              withSpinner(uiOutput(NS(id,"Overview")),type = 4),
              HTML(appConfig$footerHTML)
            )
          )
        )
       
      )
  )
  
}

OverviewServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    output$Overview <- renderUI({  
      HTML(
        paste0(
          '
          <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
          "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
  
          <body>
            
            <center><img src="./images/medicine_h_clr.png" width="600" height="auto"></center>
            
          <br /><br />
  
          <div class="overviewHeader">OVERVIEW</div>
          <div class="overviewBodyText">
          <a href="https://www.nih.gov/news-events/nih-research-matters/revealing-human-proteome" target="_blank">
            <b>Proteomics</b></a> is the study of all proteins produced by a tissue or organism. 
            In biology, proteins are the functional molecules that result when a gene is expressed and that perform most of the biological processes needed for life. 
            Many factors can affect the expression and function of proteins, and abnormal protein expression or function can play a role in health and disease. 
            Therefore, by measuring the levels of many different proteins (i.e. the &ldquo;proteome&rdquo;), 
            we can understand differences in protein levels and explore how those differences may affect health conditions. 
            We used proteomics to compare proteins in plasma from blood between people with and without Down syndrome; 
            we also made this comparison in people with Down syndrome with and without various co-occurring health conditions.
          </div>
  
          <br />
          <div class="overviewHeader">HEADING 2</div>
          <div class="overviewBodyText">
          Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
          Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
          Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
          Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
          </div>
          <br />
          <div class="overviewBodyText"><b>Effect of covid 19.</b>
          Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
          Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
          Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
          Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
          </div>
  
          
  
          <br />
          <div class="overviewHeader">METHODS</div>
          <div class="overviewBodyText">
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
            Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
            Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
            Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
          </div>
  
          <br />
          <div class="overviewHeader">PUBLICATIONS</div> 
          <div class="overviewBodyText">
          Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
          Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
          Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
          Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
          </div>
  
          <br /><br />
  
          </body>
        ')
      )
    })
    
  })
  
}