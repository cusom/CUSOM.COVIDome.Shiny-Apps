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
                <div class="overviewHeader">WHAT IS THE PROTEOME?</div>
                <p>
                <a href="https://www.nih.gov/news-events/nih-research-matters/revealing-human-proteome" target="_blank">
                  <b>Proteomics</b></a> is the study of all proteins produced by a tissue or organism. 
                  In biology, proteins are the functional molecules that result when a gene is expressed and that perform most of the biological processes needed for life. 
                  Many factors can affect the expression and function of proteins, and abnormal protein expression or function can play a role in health and disease. 
                  Therefore, by measuring the levels of many different proteins (i.e. the &ldquo;proteome&rdquo;), we can understand differences in protein levels and explore how 
                  those differences may affect health conditions. We used proteomics to compare proteins in plasma from blood between people with and without a confirmed diagnosis of COVID-19, 
                  and also between COVID-19 patients with low versus high degrees of seroconversion.
                </p>
        
                <br />
                <div class="overviewHeader">OVERVIEW</div>
                <p>
                This application contains data generated from plasma using two different platforms: Mass Spectrometry for detection of 400+ abundant proteins 
                and SOMAscan&reg; assays for detection of 5000+ epitopes corresponding to 3500+ unique proteins. 
                <br />
                <div style="margin-left: 15px;">
                  <span class="fas fa-virus"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Karyotype\')" >Effect of COVID-19 status</a></b>: Explore the differences in protein levels between samples from those with versus without COVID-19. 
                  <br />
                  <span class="fas fa-shield-virus"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Seroconversion\')" >Effect of Seroconversion</a></b>: Explore the differences in protein levels between samples from COVID-19 patients with low levels 
                  of seroconversion versus those with high levels of seroconversion.  
                  <br />
                  <span class="fas fa-viruses"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Correlates\')" >Cross Omics Correlates</a></b>: Investigate correlations between any protein of choice and any other feature present in the other datasets.                
                </div>
                <br />
                The data are searchable by protein name and can be filtered by platform, age group, and sex. 
                Users can select different statistical tests and adjustment methods for multiple hypotheses correction.
                A list of proteins detected by the Mass Spectrometry platform can be found <a href="./MS Proteomics Protein Names.pdf" target="_blank"><b>here</b></a>. 
                A list of the reagents used to detect proteins with the SOMAscan&reg; platform can be found <a href="./SOMAscan Protein Names.pdf" target="_blank"><b>here</b></a>. 
                A detailed description of how the COVIDome datasets were generated and how the COVIDome Explorer was built can be found in our manuscript under 
                <a href="https://www.medrxiv.org/content/10.1101/2021.03.04.21252945v1.full" target="_blank"><b>review</b></a>. 
                A detailed description of the analysis of the impact of seroconversion in COVID-19 can be found in our 
                <a href="https://elifesciences.org/articles/65508" target="_blank"><b>eLIFE</b></a> publication.              
                </p>
                <br />
                <div class="overviewHeader">METHODS</div>
                <p>
                Plasma was obtained from EDTA blood collection tubes. After centrifugation to separate plasma from white blood cells and red blood cells, 
                plasma aliquots were cryopreserved after the time of analysis. Mass spectrometry proteomics data was obtained by the 
                <a href="https://medschool.cuanschutz.edu/corefacilities/ms-proteomics/home" target="_blank"><b>Mass Spectrometry Proteomics Shared Resource of the University of Colorado</b></a>. 
                SOMAscan&reg; data was generated from native plasma by <a href="https://somalogic.com" target="_blank"><b>SomaLogic Inc</b></a>.
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