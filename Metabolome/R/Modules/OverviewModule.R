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
                We used metabolomics to compare metabolites between people with and without a confirmed diagnosis of COVID19.
                </p>
        
                <br />
                <div class="overviewHeader">OVERVIEW</div>
                <p>
                This dashboard presents metabolomic data generated from two different samples types: plasma and red blood cells (RBCs). 
                In the &lsquo;Effect of COVID19&rsquo; dashboard, users can explore the differences in metabolite levels between samples from those with versus without COVID19. 
                In the &lsquo;Effect of Seroconversion&rsquo; dashboard, users can explore the differences in metabolite levels between samples from COVID19 patients with low levels of seroconversion versus those with high levels of seroconversion.  
                The data are searchable by <a href="./Metabolite Names.pdf" target="_blank"><b>metabolite name</b></a> and can be filtered by age group and sex. 
                Users can select different statistical tests and adjustment methods for multiple hypotheses correction.
                A detailed description of seroconversion studies can be found in our manuscript under 
                <a href="https://www.medrxiv.org/content/10.1101/2020.12.05.20244442v1" target="_blank"><b>review</b></a>. 
                </p>
                <br />

                <div class="overviewHeader">METHODS</div>
                <p>
                Metabolomics data were generated via ultra-high-pressure liquid chromatography coupled to high-resolution mass spectrometry (UHPLC-HRMS) 
                technology by the <a href="https://medschool.cuanschutz.edu/corefacilities/ms-metabolomics/home" target="_blank"><b>Mass Spectrometry Metabolomics Shared Resource</b></a> of the university of Colorado. 
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