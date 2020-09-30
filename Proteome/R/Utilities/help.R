VolcanoHelp <- list(
  name = "Click any point on the plot below to see the a comparison of D21 versus T21",
  icon = list(
    path = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z', 
    transform = 'matrix(1 0 0 1 -1 -1) scale(0.75)'
  ),
  click = htmlwidgets::JS(
    'function(gd) {
    var helpWindow = window.open("", "HelpWindow", "width=200,height=100");
    helpWindow.document.write("<p>Click any point on the plot below to see the a comparison of D21 versus T21</p>"); 
    }'
  )
  )

BoxplotHelp <- list(
  name = "Use the box or lasso select to highlight records and see additional information below",
  icon = list(
    path = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z', 
    transform = 'matrix(1 0 0 1 -1 -1) scale(0.75)'
  ),
  click = htmlwidgets::JS(
    'function(gd) {
    var helpWindow = window.open("", "HelpWindow", "width=200,height=100");
    helpWindow.document.write("<p>Use the box or lasso select to highlight records and see additional information below</p>"); 
    }'
    )
  )

BoxplotCompareGroup <- list(
  name = "Compare Selected Groups",
  icon = list(
    path = 'M17.431,2.156h-3.715c-0.228,0-0.413,0.186-0.413,0.413v6.973h-2.89V6.687c0-0.229-0.186-0.413-0.413-0.413H6.285c-0.228,0-0.413,0.184-0.413,0.413v6.388H2.569c-0.227,0-0.413,0.187-0.413,0.413v3.942c0,0.228,0.186,0.413,0.413,0.413h14.862c0.228,0,0.413-0.186,0.413-0.413V2.569C17.844,2.342,17.658,2.156,17.431,2.156 M5.872,17.019h-2.89v-3.117h2.89V17.019zM9.587,17.019h-2.89V7.1h2.89V17.019z M13.303,17.019h-2.89v-6.651h2.89V17.019z M17.019,17.019h-2.891V2.982h2.891V17.019z', 
    transform = 'matrix(1 0 0 1 -1 -1) scale(0.75)'
  ),
  click = htmlwidgets::JS(
    "function(gd) {
      ShowBoxplotGroupOptions(gd,'BoxPlotGroupComparison');
    }"
  )
)

BoxplotClear <- list(
  name = "Clear Selected Groups",
  icon = list(
    path = 'M3.24,7.51c-0.146,0.142-0.146,0.381,0,0.523l5.199,5.193c0.234,0.238,0.633,0.064,0.633-0.262v-2.634c0.105-0.007,0.212-0.011,0.321-0.011c2.373,0,4.302,1.91,4.302,4.258c0,0.957-0.33,1.809-1.008,2.602c-0.259,0.307,0.084,0.762,0.451,0.572c2.336-1.195,3.73-3.408,3.73-5.924c0-3.741-3.103-6.783-6.916-6.783c-0.307,0-0.615,0.028-0.881,0.063V2.575c0-0.327-0.398-0.5-0.633-0.261L3.24,7.51 M4.027,7.771l4.301-4.3v2.073c0,0.232,0.21,0.409,0.441,0.366c0.298-0.056,0.746-0.123,1.184-0.123c3.402,0,6.172,2.709,6.172,6.041c0,1.695-0.718,3.24-1.979,4.352c0.193-0.51,0.293-1.045,0.293-1.602c0-2.76-2.266-5-5.046-5c-0.256,0-0.528,0.018-0.747,0.05C8.465,9.653,8.328,9.81,8.328,9.995v2.074L4.027,7.771z', 
    transform = 'matrix(1 0 0 1 -1 -1) scale(0.75)'
  ),
  click = htmlwidgets::JS(
    "function(gd) {
      clearBoxplotGroups(gd);
    }"
  )
)



# tutorials <- data.frame(
#   
#   namespace = c("Karyotype",
#                 "Karyotype",
#                 "Karyotype",
#                 "Karyotype", 
#                 "Karyotype",
#                 "Karyotype",
#                 "Karyotype",
#                 "Karyotype",
#                 "Karyotype",
#                 "Karyotype",
#                 "Karyotype", 
#                 "Karyotype", 
#                 "Karyotype", 
#                 "Karyotype",
#                 "Karyotype", 
#                 "Karyotype", 
#                 "Karyotype"
#   ),
#   tutorialName = c("DatasetOptions",
#                    "DatasetOptions",
#                    "DatasetOptions",
#                    "DatasetOptions", 
#                    "DatasetOptions",
#                    "DatasetOptions",
#                    "VolcanoPlot",
#                    "VolcanoPlot",
#                    "VolcanoPlot",
#                    "VolcanoPlot",
#                    "BoxPlot", 
#                    "BoxPlot",
#                    "BoxPlot", 
#                    "BoxPlot",
#                    "BoxPlot",
#                    "BoxplotGroupComparison", 
#                    "BoxplotGroupComparison"
#   ),
#   tutorialTitle = c("DatasetOptions",
#                     "DatasetOptions",
#                     "DatasetOptions",
#                     "DatasetOptions", 
#                     "DatasetOptions",
#                     "DatasetOptions",
#                     "Volcano",
#                     "Volcano",
#                     "Volcano",
#                     "Volcano",
#                     "BoxPlot", 
#                     "BoxPlot",
#                     "BoxPlot", 
#                     "BoxPlot",
#                     "BoxPlot",
#                     "BoxplotGroupComparison", 
#                     "BoxplotGroupComparison"
#   ),   
#   element = c("#Dataset-Options.sidebar-text",
#               "#Karyotype-Platform",
#               "#Karyotype-Sex", 
#               ".sidebar-menu > div:nth-child(7) > div:nth-child(5) > label:nth-child(1)", 
#               ".sidebar-menu > div:nth-child(7) > div:nth-child(6)",
#               "#Karyotype-VolcanoDatasetRefresh",
#               # VOLCANO 
#               "#Karyotype-VolcanoPlot",
#               "#Karyotype-VolcanoPlot.plotly.html-widget.html-widget-output.shiny-report-size.shiny-bound-output.js-plotly-plot div.plot-container.plotly div.svg-container svg.main-svg g.cartesianlayer g.subplot.xy g.plot g.scatterlayer.mlayer",
#               "div.form-group:nth-child(6) > label:nth-child(1)",
#               "#Karyotype-VolcanoPlot-modebar",
#               # BOXPLOT 
#               "#Karyotype-AnalyteBoxPlot", # show whole plot
#               "#Karyotype-AnalyteBoxPlot-modebar.modebar", #show modebar
#               "#Karyotype-AnalyteBoxPlot-modebar > div:nth-child(1) > a:nth-child(1)", #box select
#               "#Karyotype-AnalyteBoxPlot-modebar > div.modebar-group:nth-child(2) > a:nth-child(1)", #lasso select
#               "#Karyotype-AnalyteBoxPlot-modebar > div.modebar-group:nth-child(3) > a:nth-child(1)", #camera
#               #BoxplotGroupComparison
#               "#Karyotype-AnalyteBoxPlot-modebar > div.modebar-group:nth-child(4) > a:nth-child(1)", #show comparison 
#               "#Karyotype-AnalyteBoxPlot-modebar > div.modebar-group:nth-child(5) > a:nth-child(1)"  #clear comparsion
#   ),
#   intro = c("To render the volcano plot, start by setting dataset options below:",
#             "Choose a Platform.", 
#             "Optionally select Sex(es)", 
#             "Optionally select a range of Ages", 
#             "Optionally choose an Analyte to highlight in the Volcano Plot - if you set this, an arrow will point out the given analyte on the volcano plot", 
#             "When you have set your filters, click this button to generate the volcano plot...",
#             
#             # VOLCANO 
#             "Volcano plots are a form of scatterplot used to indicate statistical significance
#                  (p-value<0.05 are above the dashed line) in comparison to the fold change (upregulated genes are to the right of the y axis).",
#             "Click on any dot to choose an analyte and highlight it on the volcano plot - this will also create a boxplot showing the difference between D21 and T21 for the chosen analyte ",
#             "Or, choose an analyte from this dropdown list to highlight it on the volcano plot - this will also create a boxplot showing the difference between D21 and T21 for the chosen analyte ",
#             "Use these tools to interact further with the plot (download images, select points, etc)",
#             # BOXPLOT 
#             "This boxplot shows the difference between T21 and D21 for the chosen analyte",
#             "Use these widgets to further interact with this plot:",
#             "Use the Box Select tool to select distinct sets or groups of observations",
#             "Alternatively, use the Lasso Select tool to select sets or groups with finer precision" ,
#             "Use this widget to download a .png of this plot" ,
#             #BoxplotGroupComparison
#             "Now that you have highlighted 2 distinct groups, click this to see options to compare the sets", 
#             "Click this button to clear your selections from this plot. "
#   ),
#   position = c("right",
#                "right",
#                "right",
#                "right", 
#                "right",
#                "right",
#                #
#                "top",
#                "right",
#                "right",
#                "right",
#                #
#                "top", 
#                "top", 
#                "top", 
#                "top", 
#                "top", 
#                #
#                "top", 
#                "top"
#                 )
# )



#saveRDS(tutorials,'./Data/tutorials.rds')
