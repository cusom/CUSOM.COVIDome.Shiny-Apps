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
                <div class="overviewHeader">WHAT IS THE TRANSCRIPTOME?</div>
                <p>
                  <a href="https://www.genome.gov/about-genomics/fact-sheets/Transcriptome-Fact-Sheet" target="_blank"><b>Transcriptomics</b></a> is the study of ribonucleic acid (RNA), a molecule that is produced in a cell when a gene is turned on. 
                  While the DNA that encodes all genes is the same in every cell, different types of cells (e.g. a heart cell versus a liver cell) may turn certain genes on 
                  and off at different times. Other factors may also affect what genes are turned on and off, such as age, gender, and diseases or other health conditions. 
                  Therefore, by measuring the levels of all RNAs (i.e. the "transcriptome"), we can understand what genes are expressed in a variety of situations. 
                  We used transcriptomics to compare gene expression in the bloodstream of people with and without a confirmed diagnosis of COVID-19.
                </p>
                <br />

                <div class="overviewHeader">OVERVIEW</div>
                <p>
                This application presents transcriptome data generated from whole blood. 
                <br />
                <div style="margin-left: 15px;">
                  <span class="fas fa-virus"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Karyotype\')" >Effect of COVID-19 status</a></b>: Explore the differences in gene expression levels between samples from those with versus without COVID-19. 
                  <br />
                  <span class="fas fa-shield-virus"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Seroconversion\')" >Effect of Seroconversion</a></b>: Explore the differences in gene expression levels between samples from COVID-19 patients with low levels of seroconversion 
                  versus those with high levels of seroconversion. 
                  <br />
                  <span class="fas fa-viruses"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Correlates\')" >Cross Omics Correlates</a></b>: Investigate correlations between any gene of choice and any other feature present in the other datasets.               
                </div>
                <br />
                The data are searchable by <a href="./Transcriptome Gene Names.pdf" target="_blank"><b>gene name</b></a> and can be filtered by age group and sex. 
                Users can select different statistical tests and adjustment methods for multiple hypotheses correction. 
                A detailed description of how the COVIDome datasets were generated and how the COVIDome Explorer was built can be found in our manuscript under 
                <a href="https://www.medrxiv.org/content/10.1101/2021.03.04.21252945v1.full" target="_blank"><b>review</b></a>. 
                A detailed description of the analysis of the impact of seroconversion in COVID-19 can be found in our 
                <a href="https://elifesciences.org/articles/65508" target="_blank"><b>eLIFE</b></a> publication.              
                </p>
        
                <br />
                <div class="overviewHeader">METHODS</div>
                <p>
                  Blood was drawn into a PAXgene RNA tubes, RNA was extracted, and next-generation sequencing libraries were generated 
                  and subjected to deep sequencing at the 
                  <a href="https://medschool.cuanschutz.edu/colorado-cancer-center/research/shared-resources/genomics" target="_blank">
                    <b>Genomics and Microarray Shared Resource of the University of Colorado.</b></a> 
                </p>
                <br />

                <div class="overviewHeader">Tutorials and Guides</div>
                Please look out for
                <span
                  data-placement="top" 
                  data-toggle="tooltip"               
                  class="fas fa-info-circle gtooltip"
                  style="color:#1e8bf0; font-size:16px;"
                  data-original-title="Click me to launch tutorials or see additional information!">
                </span>            
                icons to learn more about how to use the various dashboards and how to interpret the diverse plots generated
          
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