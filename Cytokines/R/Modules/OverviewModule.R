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
                '<div class="overviewHeader">WHAT ARE CYTOKINES?</div>
                <p>
                Cytokines, chemokines and related factors are proteins employed by the immune system to orchestrate appropriate immune responses. 
                These proteins are key for communication among various tissues and immune cells, and for modulation of immune cell function. 
                Given their importance in COVID-19, cytokines, chemokines and related immune factors were measured with a technology exclusively devoted 
                to the detection of these molecules in a quantitative fashion.
                </p>
        
                <br />
                <div class="overviewHeader">OVERVIEW</div>
                <p>
                This application contains data generated from plasma using Meso Scale Discovery&reg; assays for 82 unique immune regulatory factors. 
                <br />
                <div style="margin-left: 15px;">
                  <span class="fas fa-virus"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Karyotype\')" >Effect of COVID-19 status</a></b>: Explore the differences in cytokine levels 
                  between samples from those with versus without COVID-19. 
                  <br />
                  <span class="fas fa-shield-virus"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Seroconversion\')" >Effect of Seroconversion</a></b>: Explore the differences in cytokine levels between samples from COVID-19 patients with low levels 
                  of seroconversion versus those with high levels of seroconversion.  
                  <br />
                  <span class="fas fa-viruses"></span><b> <a title="Click to navigate to tab" onclick="openTab(\'Correlates\')" >Cross Omics Correlates</a></b>: Investigate correlations between any cytokine of choice and any other feature present in the other datasets.
                </div>
                <br />
                The data are searchable by <a href="./MSD Names.pdf" target="_blank"><b>factor name</b></a> and can be filtered by age group and sex. 
                Users can select different statistical tests and adjustment method for multiple hypotheses correction. 
                
                A detailed description of how the COVIDome datasets were generated and how the COVIDome Explorer was built can be found in our manuscript under 
                <a href="https://www.medrxiv.org/content/10.1101/2021.03.04.21252945v1.full" target="_blank"><b>review</b></a>. 
                A detailed description of the analysis of the impact of seroconversion in COVID-19 can be found in our 
                <a href="https://elifesciences.org/articles/65508" target="_blank"><b>eLIFE</b></a> publication.              
                </p>
                <br />

                <div class="overviewHeader">METHODS</div>
                <p>
                Meso Scale Discovery&reg; data was generated by the <a href="https://medschool.cuanschutz.edu/immunology-immunotherapy/himsr" target="_blank"><b>Human Immune Monitoring Shared Resource of the University of Colorado</b></a> 
                using manufacturer&lsquo;s instructions. 
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