stdCurve <- function() {
  shinydashboard::tabItem(tabName = "stdCurve",
    shiny::h1(shiny::strong("Standard Curve Analysis")),
    fluidRow(
      box(id = "tabset1",width = 3,height = "600px",
        fluidRow(column(12,
          br(),
          shiny::p("To analyze standard curve data, upload a qPCR standard curve
          fluorescence file, and a filled in metadata file.",style = "font-size:16px;"),
          numericInput(inputId = "LOQthres",label="LOQ Coefficient of Variation Threshold",
                       value=0.35,min=0,max=0.99),
          br(),
          shiny::p("Remove wells that do not have amplification."),
          selectInput(inputId = "SC_wells",label = "Wells to Include",
                      choices = "None",selected = "None",size=12,
                      multiple = T,selectize=F),
          actionButton("std_recalib","Calibrate Curve"),
          br(),
          box(width = NULL, solidHeader = TRUE,collapsible = TRUE, collapsed = F,
              title = "LOD Calculation Messages",
              status = "warning",column(12, textOutput("text")))
        ))# end of fluid row
      ),#end of box

      #Standard curve plot
      tabBox(title = shiny::strong("Standard Curve Analysis"),id = "data_analysis_box",
             height = "800px",width = 9,
        tabPanel("Standard Curve Data Overview",
          shiny::strong("Data from the 'Standard Curve Data Overview' table
                        is visualized in the 'Standard Curve Plot' tab."),
          DT::dataTableOutput('SC_overview_table')
        ),
        tabPanel("Standard Curve Plot",
          shiny::strong("The residual gradient colour scheme depicts how well
                        the standard curve fits the data points. Yellow points
                        are best fit by the curve, dark purple points are least
                        fit, and orange points are in between."), p("LOD =
                        Limit of Detection, LOQ = Limit of Quantification"),
          plotly::plotlyOutput("standardCurve_plot")
        )#End of tab panel
      )# end of tab box
    ) #end of fluid row
  )# end of shiny dashboard tab item
}#End of function
