#' @import shinydashboard
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @import leaflet
#' @import leaflet.extras
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyjs reset
#' @importFrom shinyjs alert
#' @importFrom shinyjs useShinyjs
#' @import ggplot2
#' @import dplyr
#' @import readxl
#' @import reactable
#' @import writexl
#' @importFrom xfun file_ext
#' @importFrom berryFunctions is.error
#' @importFrom plotly plotlyOutput
#' @importFrom plotly style
#' @importFrom plotly layout
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @import htmltools
#' @importFrom shiny div
#' @importFrom shiny downloadHandler
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny need
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveVal
#' @importFrom shiny renderText
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny validate
#' @importFrom shiny a
#' @importFrom shiny actionButton
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny downloadLink
#' @importFrom shiny em
#' @importFrom shiny fileInput
#' @importFrom shiny fluidRow
#' @importFrom shiny h1
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny p
#' @importFrom shiny radioButtons
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny strong
#' @importFrom shiny tabPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny shinyApp
#' @importFrom shiny textOutput
#' @import methods
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils zip
#' @importFrom stats family
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats na.exclude
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats residuals
#' @importFrom bslib is_bs_theme
NULL
stdCurve <- function() {
  shinydashboard::tabItem(tabName = "stdCurve",
                          shiny::h1(shiny::strong("Standard Curve Analysis")),
          fluidRow(
            tabBox(id = "tabset1",
                   width = 3,
                   height = "600px",

                   #Tab1 containing the standard curve files
                   tabPanel("Upload Files",

                            fluidRow(column(12,

                                            br(),

                                            shiny::p("To analyze standard curve data, upload a qPCR standard curve fluorescence file, and a filled in metadata file.",  style = "font-size:16px;"),


                                            fileInput("SC_fluorescence_file",
                                                      "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
                                                      multiple = FALSE,
                                                      accept = c(".xlsx", ".xls", ".csv")),

                                            fileInput("SC_metadata_file",
                                                      "Upload Metadata File (xlsx/xls)",
                                                      multiple = FALSE,
                                                      accept = c(".xlsx", ".xls")),

                                            #Select qPCR run platform
                                            selectInput(inputId = "SC_platform",
                                                        label = "qPCR Platform",
                                                        choices = c("None",
                                                                    "StepOnePlus",
                                                                    "Biomeme two3/Franklin",
                                                                    "MIC/BioRad"),
                                                        multiple = FALSE),

                                            numericInput(inputId = "LOQthres",
                                                         label="LOQ Coefficient of Variation Threshold",
                                                         value=0.35,
                                                         min=0,
                                                         max=0.99),


                                            #Uploaded files submit and reset button

                                            fluidRow(column(6, actionButton("Uploaded_SC_submit",
                                                                            "Submit  Files")),
                                                     column(6, actionButton("Uploaded_SC_reset",
                                                                            "Reset Files"))),
                                            br(),
                                            br(),

                                            box(width = NULL, solidHeader = TRUE,
                                                collapsible = TRUE, collapsed = F, title = "LOD Calculation Messages",
                                                status = "warning",
                                                column(12, textOutput("text"))),
                            ))),
                   tabPanel("Design Std Curve",

                            column(12,

                                   br(),
                                   shiny::p("Remove wells that do not have amplification."),
                                   #uiOutput("selectComp"),
                                   selectInput(inputId = "SC_wells",
                                               label = "Wells to Include",
                                               choices = "None",
                                               selected = "None",
                                               size=12,
                                               multiple = T,
                                               selectize=F),

                                   fluidRow(column(4,
                                                   actionButton("std_recalib",
                                                                "Recalibrate Curve!")))

                            )),
                   tabPanel("Low Quant eDNA",

                            column(12,

                                   br(),
                                   shiny::p("Select a false positive rate and false negative rate for the calcuation of LOB, LOD and LOQ."),
                                   fluidRow(column(4,
                                                   actionButton("lowquant_submit",
                                                                "Calculate Low Quantity LOD/LOQ Measures")))

                            ))
            ),
            #Standard curve plot
            tabBox(

              title = shiny::strong("Standard Curve Analysis"),
              id = "data_analysis_box",
              height = "800px",
              width = 9,

              tabPanel("Standard Curve Data Overview",

                       shiny::strong("Data from the 'Standard Curve Data Overview' table is visualized in the 'Standard Curve Plot' tab."),

                       DT::dataTableOutput('SC_overview_table')),

              tabPanel("Standard Curve Plot",
                       shiny::strong("The residual gradient colour scheme depicts how well the standard curve fits the data points. Yellow points are best fit by the curve, dark purple points are least fit, and orange points are in between."), p("LOD = Limit of Detection, LOQ = Limit of Quantification"),

                       plotly::plotlyOutput("standardCurve_plot"))

            )# end of tab box
          ) #end of fluid row
  )# end of tab item

}
