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
              HTML(
                ' 
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
                In the &lsquo;Effect of COVID19&rsquo; dashboard, users can explore the differences by &lsquo;parent cell lineage&rsquo; between samples from those with versus without COVID19. 
                In the &lsquo;Effect of Seroconversion&rsquo; dashboard, users can explore the differences by &lsquo;parent cell lineage&rsquo; between samples from COVID19 patients with low levels of seroconversion versus those with high levels of seroconversion.  
                The data are searchable by seven immune submaps defined by the &lsquo;parent cell lineage&rsquo; and can be filtered by age group and sex. 
                Users can select different statistical tests and adjustment method for multiple hypotheses correction. 
                A detailed description of seroconversion studies can be found in our manuscript under 
                <a href="https://www.medrxiv.org/content/10.1101/2020.12.05.20244442v1" target="_blank"><b>review</b></a>. 
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
      
                <br /><br />'
              )
            )
          )
        )
       
      )
  )
  
}

OverviewServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    
  })
  
}