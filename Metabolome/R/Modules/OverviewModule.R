

OverviewUI <- function(id) {
  list(
    "Inputs" = 
      list(

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
          <a href="https://en.wikipedia.org/wiki/Metabolomics" target="_blank"><b>Metabolomics</b></a> is the study of all small molecules, or &ldquo;metabolites,&rdquo; produced by a cell, tissue, 
          or organism while performing typical biological processes, such as converting food into energy, or getting rid of waste products from cells. 
          Together, these metabolites create a chemical fingerprint (i.e. the &ldquo;metabolome&rdquo;) that provides valuable information about how that cell, tissue, 
          or organism is functioning. Therefore, by measuring all metabolites, we can explore how cells are actively functioning in response to both internal 
          gene expression changes and external environmental factors. 
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