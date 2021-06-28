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
              HTML('
                <p>
                The COVIDome dataset was generated from research participants hospitalized at Children&rsquo;s Hospital Colorado (CHCO) or 
                the University of Colorado Hospital (UCH) at the University of Colorado Anschutz Medical Campus in Aurora, Colorado, United States of America.
                <br />
                <br />
                The first batch of COVIDome data was produced from a cohort involving 105 total participants, 73 of them with confirmed SARS-CoV-2 infection 
                at the time of blood draw, referred hereinto as COVID19+. The male to female ratio is approximately 3:2 in both the COVID negative and COVID19+ groups. 
                In the COVID19+ group, all participants were symptomatic at the time of blood draw, but none of them had symptoms severe enough to justify 
                admission into an intensive care unit (ICU). No blood draws were obtained from patients in the ICU for this study. 
                <br />
                <br />
                Blood was collected into EDTA (purple top), Heparin (green top) and PAXgene RNA tubes. From EDTA tubes, plasma was analyzed to produce the plasma proteome, 
                plasma metabolome, and cytokine profile datasets. From the same EDTA tube, red blood cells were used to obtain the red blood cell metabolome dataset. 
                From the matched Heparin tube, peripheral mononuclear blood cells (PBMCs) were purified to generate the immune maps. From the PAX gene RNA tubes, 
                RNA was extracted to obtain the transcriptome dataset. Therefore, all datasets generated and presented here are from matched blood samples.
                <br />
                <br />
                You can read a detailed description of the COVIDome dataset and the building of the COVIDome Explorer in our paper under review 
                <b><a href="https://www.medrxiv.org/content/10.1101/2021.03.04.21252945v1" target="_blank">here</a></b> 
                <br />
                <br />
                You can read our paper in press describing how seroconversion stages COVID19 pathology into distinct pathophysiological states 
                <b><a href="https://www.medrxiv.org/content/10.1101/2020.12.05.20244442v1.full" target = "blank">here</a></b>

                <br />
                <br />
                <center>
                  <object data="images/Experimental design-01.svg" type="image/svg+xml"></object>
                </center>
                </p> 
              ')
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