

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
          '<br /><br />
          
          <div class="overviewHeader">WHAT IS AN IMMUNE MAP?</div>
          <p>
          An immune map is a detailed characterization of the types and amounts of immune cells present in an organism. 
          Altogether, these immune cells drive the immune response against infectious agents such as bacteria and viruses. 
          Some immune cells also participate in the defense against tumors, and some can contribute to the appearance and severity of autoimmune disorders. 
          Therefore, an immune map provides valuable information about how the immune system is functioning. 
          By measuring a multitude of immune cell types in the blood, we can explore how the immune system is different in people with and without a 
          confirmed diagnoses of COVID19.
          </p>
  
          <br />
          <div class="overviewHeader">OVERVIEW</div>
          <p>
          This dashboard contains data generated from circulating blood using a platform known as mass cytometry or cytometry time-of-flight (CyTOF). 
          CyTOF enables the measurement of hundreds of immune cell subsets. 
          In this dashboard, users can select one of seven immune submaps defined by the &lsquo;parent cell lineage&rsquo; and visualize 
          individual cell types within that lineage.
          </p>

          <br />
          <div class="overviewHeader">METHODS</div>
          <p>
          Peripheral mononuclear blood cells (PBMCs) were isolated from blood using the Ficoll gradient technique. 
          A detailed protocol of the fixing and staining procedures can be found <a href="./CyTOF Methods.pdf" target="_blank"><b>here</b></a>. 
          Mass cytometry was done at the 
          <a href="https://medschool.cuanschutz.edu/colorado-cancer-center/research/shared-resources/flow-cytometry" target="_blank">
          <b>Flow Cytometry Shared Resource of the University of Colorado Cancer Center</b></a>. 
          A table listing the antibodies employed can be found <a href="./CyTOF Antibodies.pdf" target="_blank"><b>here</b></a>. 
          A table explaining cell type definition based on expression markers can be found <a href="./CyTOF Definitions.pdf" target="_blank"><b>here</b></a>
          </p>         
  
          <br /><br />

        ')
      )
    })
    
  })
  
}