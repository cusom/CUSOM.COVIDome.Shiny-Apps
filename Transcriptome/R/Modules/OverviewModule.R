

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
  
          <div class="overviewHeader">WHAT IS THE TRANSCRIPTOME?</div>
          <p>
            <a href="https://www.genome.gov/about-genomics/fact-sheets/Transcriptome-Fact-Sheet" target="_blank"><b>Transcriptomics</b></a> is the study of ribonucleic acid (RNA), a molecule that is produced in a cell when a gene is turned on. 
            While the DNA that encodes all genes is the same in every cell, different types of cells (e.g. a heart cell versus a liver cell) may turn certain genes on 
            and off at different times. Other factors may also affect what genes are turned on and off, such as age, gender, and diseases or other health conditions. 
            Therefore, by measuring the levels of all RNAs (i.e. the "transcriptome"), we can understand what genes are expressed in a variety of situations. 
            We used transcriptomics to compare gene expression in the bloodstream of people with and without a confirmed diagnosis of COVID19.
          </p>
          <br />

          <div class="overviewHeader">OVERVIEW</div>
          <p>
            This dashboard presents transcriptome data generated from whole blood. 
            The data are searchable by <a href="./Transcriptome Gene Names.pdf" target="_blank"><b>gene name</b></a> and can be filtered by age group and sex. 
            Users can select different statistical tests and adjustment methods for multiple hypotheses correction. 
          </p>
  
          <br />
          <div class="overviewHeader">METHODS</div>
          <p>
            Blood was drawn into a PAXgene RNA tubes, RNA was extracted, and next-generation sequencing libraries were generated 
            and subjected to deep sequencing at the 
            <a href="https://medschool.cuanschutz.edu/colorado-cancer-center/research/shared-resources/genomics" target="_blank">
              <b>Genomics and Microarray Shared Resource of the University of Colorado.</b></a> 
          </p>
  
          <br /><br />

        ')
      )
    })
    
      
  })
  
}