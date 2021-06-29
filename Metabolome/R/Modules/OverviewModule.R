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
                <div class="overviewHeader">WHAT IS THE METABOLOME?</div>
                <p>
                <a href="https://en.wikipedia.org/wiki/Metabolomics" target="_blank"><b>Metabolomics</b></a> is the study of all small molecules, 
                or &ldquo;metabolites&rdquo;, produced by a cell, tissue, or organism while performing typical biological processes, such as converting food into energy, 
                or getting rid of waste products from cells. Together, these metabolites create a chemical fingerprint (i.e. the &ldquo;metabolome&rdquo;) that provides 
                valuable information about how that cell, tissue, or organism is functioning. Therefore, by measuring all metabolites, we can explore how cells 
                are actively functioning in response to both internal gene expression changes and external environmental factors. 
                We used metabolomics to compare metabolites between people with and without a confirmed diagnosis of COVID-19.
                </p>
        
                <br />
                <div class="overviewHeader">OVERVIEW</div>
                <p>
                This application presents metabolomic data generated from two different samples types: plasma and red blood cells (RBCs). 
                <br />
                <div style="margin-left: 15px;">
                  <span class="fas fa-virus"></span><b> Effect of COVID-19 status</b>: Explore the differences in metabolite levels between samples from those with versus without COVID-19. 
                  <br />
                  <span class="fas fa-shield-virus"></span><b> Effect of Seroconversion</b>: Explore the differences in metabolite levels between samples from COVID-19 patients with low levels of seroconversion versus those with high levels of seroconversion.  
                  <br />
                  <span class="fas fa-viruses"></span><b> Cross Omics Correlates</b>: Investigate correlations between any metabolite of choice and any other feature present in the other datasets.
                </div>
                <br />
                The data are searchable by <a href="./Metabolite Names.pdf" target="_blank"><b>metabolite name</b></a> and can be filtered by age group and sex. 
                Users can select different statistical tests and adjustment methods for multiple hypotheses correction.
                A detailed description of how the COVIDome datasets were generated and how the COVIDome Explorer was built can be found in our manuscript under 
                <a href="https://www.medrxiv.org/content/10.1101/2021.03.04.21252945v1.full" target="_blank"><b>review</b></a>. 
                A detailed description of the analysis of the impact of seroconversion in COVID-19 can be found in our 
                <a href="https://elifesciences.org/articles/65508" target="_blank"><b>eLIFE</b></a> publication.              
                </p>
                <br />

                <div class="overviewHeader">METHODS</div>
                <p>
                Metabolomics data were generated via ultra-high-pressure liquid chromatography coupled to high-resolution mass spectrometry (UHPLC-HRMS) 
                technology by the <a href="https://medschool.cuanschutz.edu/corefacilities/ms-metabolomics/home" target="_blank"><b>Mass Spectrometry Metabolomics Shared Resource</b></a> of the University of Colorado. 
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
                icons to learn more about how to use the various dashboards and how to interpret the diverse plots generated.
          
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