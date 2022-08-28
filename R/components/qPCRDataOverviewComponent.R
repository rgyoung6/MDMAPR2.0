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
#qPCR Data Overview page ---------------------------

qPCRDataOverviewComponent <- function() {
  shinydashboard::tabItem(tabName = "qPCRDataOverviewPage",

                          shiny::h1(shiny::strong("qPCR Data Overview")),

                          shiny::p("Analyze individual well samples for a qPCR run.",
            style = "font-size:16px;"),

          br(),

          #Dropdown menu to select standard curve to view
          fluidRow(

            tabBox(id = "tabset1",
                   width = 3,

                   #Tab1
                   tabPanel("Data",
                            shiny::column(12,

                                   br(),


                                   shiny::p("To analyze qPCR experimental data and standard curve data associated with a specific project please select Assay, then Machine Type, then Project Name, and then Standard Curve. Then Press the 'Submit' button.",  style = "font-size:16px;"),

                                   #Dropdown menu for assay name
                                   pickerInput(inputId = "DA_assay_input",
                                               "Assay",
                                               choices = "None",
                                               selected = "None",
                                               multiple = FALSE),


                                   #Dropdown menu for machine type
                                   pickerInput(inputId = "DA_machine_input",
                                               "Machine Type",
                                               choices = "None",
                                               selected = "None",
                                               multiple = FALSE),


                                   #Dropdown menu for project
                                   pickerInput(inputId = "DA_project_input",
                                               "Project Name",
                                               choices = "None",
                                               selected = "None",
                                               multiple = FALSE),


                                   # #Dropdown menu for standard curve
                                   # pickerInput(inputId = "SC_input",
                                   #             "Standard Curve",
                                   #             choices = "None",
                                   #             selected = "None",
                                   #             multiple = FALSE),
                                   #
                                   # br(),


                                   #Submit button and reset button
                                   shiny::fluidRow(shiny::column(4,
                                                   actionButton("DA_submit",
                                                                "Submit  Files")),
                                                   shiny::column(4,
                                                   offset = 3,
                                                   actionButton("DA_reset",
                                                                "Reset Files")))))),

            #Standard curve plot
            tabBox(

              title = shiny::strong("Data Analysis"),
              id = "data_analysis_box",
              height = 1000,
              width = 9,

              tabPanel("Presence/Absense Samples",

                       shiny::strong("Select a radio button to view associated amplification curve on 'Amplification Plot' tab."), shiny::p("Cq Value cells are coloured based on the Cq Cutoff value. Cells coloured in orange refer to positive target sequence detections and cells coloured in blue refer to negative detections."),

                       numericInput(inputId = "cqValueCutoff",
                                    label = h3("Enter Cq Cutoff Value",
                                               style = "font-size:15px;"),
                                    value = 40,
                                    min = 1,
                                    max = 40),

                       reactable::reactableOutput("presence_absence_table")),

              tabPanel("Amplification Plot",  plotly::plotlyOutput("selected"))
            )))
}
