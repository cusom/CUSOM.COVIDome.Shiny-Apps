

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
          <br /><br />
          <p>
          The COVIDome dataset was generated from research participants hospitalized at Children&lsquo;s Hospital Colorado (CHCO) or the 
          University of Colorado Hospital (UCH) at the University of Colorado Anschutz Medical Campus in Aurora, Colorado, United States of America, 
          between April and July of 2020.
          <br />
          <br />
          This cohort involves 109 total participants, 72 of them with confirmed SARS-CoV-2 infection at the time of blood draw, referred hereinto as COVID19+. 
          The male to female ratio is approximately 3:2 in both the COVID negative and COVID19+ groups. In the COVID19+ group, all participants were symptomatic 
          at the time of blood draw, but none of them had symptoms severe enough to justify admission into an intensive care unit (ICU). 
          No blood draws were obtained from patients in the ICU for this study.
          <br />
          <br />
          Blood was collected into EDTA (purple top), Heparin (green top) and PAXgene RNA tubes. 
          From EDTA tubes, plasma was analyzed to produce the plasma proteome, plasma metabolome, and cytokine profile datasets. 
          From the same EDTA tube, red blood cells were used to obtain the red blood cell metabolome dataset. 
          From the matched Heparin tube, peripheral mononuclear blood cells (PBMCs) were purified to generate the immune maps. 
          From the PAX gene RNA tubes, RNA was extracted to obtain the transcriptome dataset. 
          Therefore, all datasets generated and presented here are from matched blood samples.
          </p> 
        ')
      )
    })
    
  })
  
}