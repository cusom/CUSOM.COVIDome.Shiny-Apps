

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
              withSpinner(uiOutput(NS(id,"Overview")),type = 4)
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
          '<br /><br />
  
          <div class="overviewHeader">WHAT IS THE PROTEOME?</div>
          <p>
          <a href="https://www.nih.gov/news-events/nih-research-matters/revealing-human-proteome" target="_blank">
            <b>Proteomics</b></a> is the study of all proteins produced by a tissue or organism. 
            In biology, proteins are the functional molecules that result when a gene is expressed and that perform most of the biological processes needed for life. 
            Many factors can affect the expression and function of proteins, and abnormal protein expression or function can play a role in health and disease. 
            Therefore, by measuring the levels of many different proteins (i.e. the &ldquo;proteome&rdquo;), we can understand differences in protein levels and explore how 
            those differences may affect health conditions. 
            We used proteomics to compare proteins in plasma from blood between people with and without a confirmed diagnosis of COVID19.
          </p>
  
          <br />
          <div class="overviewHeader">OVERVIEW</div>
          <p>
          This dashboard contains data generated from plasma using two different platforms: Mass Spectrometry for detection of 400+ abundant proteins 
          and SOMAscan&reg; assays for detection of 5000+ epitopes corresponding to 3500+ unique proteins. Users can explore the differences in protein levels 
          between samples from those with versus without COVID19. The data are searchable by protein name and can be filtered by platform, age group, and sex. 
          Users can select different statistical tests and adjustment methods for multiple hypotheses correction.
          A list of proteins detected by the Mass Spectrometry platform can be found <a href="./MS Proteomics Protein Names.pdf" target="_blank"><b>here</b></a>. 
          A list of the reagents used to detect proteins with the SOMAscan&reg; platform can be found <a href="./SOMAscan Protein Names.pdf" target="_blank"><b>here</b></a>. 
          </p>
          <br />
          <div class="overviewHeader">METHODS</div>
          <p>
          Plasma was obtained from EDTA blood collection tubes. After centrifugation to separate plasma from white blood cells and red blood cells, 
          plasma aliquots were cryopreserved after the time of analysis. Mass spectrometry proteomics data was obtained by the 
          <a href="https://medschool.cuanschutz.edu/corefacilities/ms-proteomics/home" target="_blank"><b>Mass Spectrometry Proteomics Shared Resource of the University of Colorado</b></a>. 
          SOMAscan&reg; data was generated from native plasma by <a href="https://somalogic.com" target="_blank"><b>SomaLogic Inc</b></a>.
          </p>
  
          <br /><br />
  
        ')
      )
    })
    
  })
  
}