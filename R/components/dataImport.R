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

dataImport <- function() {
  shinydashboard::tabItem(tabName = "dataImport",
                          shiny::h1(shiny::strong("Data Import")),
          shiny::fluidRow(
            shiny::column(width = 6,
                          shinydashboard::box(title = htmltools::span(shiny::icon("upload"), "Upload Data"), status = "info",
                       width = 12, solidHeader = TRUE,collapsible = T, collapsed = F,
                       #qPCR Data file upload
                       shiny::p("Upload files here if you are uploading data for the first time in MDMAPR."),
                       #Upload qPCR experimental fluorescence file
                       shiny::fileInput("qpcr_file",
                                 "Upload qPCR Experimental Fluorescence File (RDML)",
                                 multiple = FALSE,
                                 accept = c(".rdml"), width="500px"),
                       shiny::fileInput("SCI_fluorescence_file",
                                 "Upload Standard Curve Fluorescence File (RDML)",
                                 multiple = FALSE,
                                 accept = c(".rdml"), width = "500px"),

                       #Upload metadata file
                       shiny::fileInput("metadata_file", "Upload Filled MDMAPR Metadata File (xlsx)", width="500px",
                                 multiple = FALSE,
                                 accept = c(".xlsx", ".xls")),

                       shiny::textInput(inputId = "upload_data_name", label = "Upload Data Name", width="450px"),

                       shiny::fluidRow(shiny::column(12,textOutput("error_msg"))),
                       shiny::fluidRow(
                         shiny::column(6, actionButton("submit",
                                                "Submit  Files")),
                         shiny::column(6, actionButton("reset",
                                                "Reset Files")))
                   )),# end of upload data box and column
            shiny::column(width = 6,
                          shinydashboard::box(title = span( icon("plus-square"), "Add Another Dataset"), collapsible = TRUE, collapsed = TRUE,
                       status = "info", width = 12, solidHeader = TRUE,
                       #qPCR Data file upload
                       shiny::p("Upload files here if you would like to add to your dataset or progress file. Only submit files here if you've already uploaded data."),
                       #Upload qPCR experimental fluorescence file
                       fileInput("qpcr_file_add",
                                 "Upload qPCR Experimental Fluorescence File (RDML)",
                                 multiple = FALSE,
                                 accept = c(".rdml"), width="500px"),
                       # add standard curve
                       fileInput("SCI_fluorescence_file_add",
                                 "Upload Standard Curve Fluorescence File (RDML)",
                                 multiple = FALSE,
                                 accept = c(".rdml"), width = "500px"),

                       #Upload metadata file
                       fileInput("metadata_file_add", "Upload Filled MDMAPR Metadata File (xlsx)",
                                 multiple = FALSE,
                                 accept = c(".xlsx", ".xls"), width="500px"),

                       textInput(inputId = "add_data_name", label = "Add Data Name", width="450px"),
                       fluidRow(column(10,textOutput("error_msg_add"))),
                       fluidRow(column(4, actionButton("submit_add",
                                                       "Submit  Files")),
                                column(4, actionButton("reset_add",
                                                       "Reset Files")))
                   ), # end of add dataset box
                   shinydashboard::box(title=span( icon("save"), "Load Saved Data"), status="warning", solidHeader=TRUE, width=12,collapsible = T, collapsed = T,
                       shiny::fluidRow(
                         column(12, h4(icon("database"), "Continue Previous Work"))),
                       fluidRow(column(12,
                                       shiny::p("Restore progress by adding an MDMAPR data file."),
                                       fileInput("load_saved_data", label = "Restore Progress (.zip file)",
                                                 buttonLabel = "Browse",
                                                 placeholder = "Previously saved MDMAPR zip file", multiple=F,
                                                 accept=c(".zip"),
                                                 width="500px"),
                                       column(4, actionButton("submit_zip",
                                                              "Submit  File"))))), # end of load data box

                   shinydashboard::box(title = span( icon("play-circle"), "Import Sample Data from MDMAPR package"),
                       collapsible = TRUE, collapsed = TRUE,
                       status = "primary", solidHeader = TRUE, width = 12,
                       fluidRow(
                         column(12, DT::DTOutput("data_set_table")),
                         column(2, offset = 0, actionButton("load_MDMAPR_data", "Load",
                                                            icon = icon("bolt")))
                       )) #end of import sample data box
            )), # end of right hand column
          #adding another section for the Data Exporting Features
          shiny::h1(shiny::strong("Data Export")),
          fluidRow(
            shinydashboard::box(width=12, status="warning",title=span( icon("file-export"), "Preview and Export Data"), solidHeader = T,
                shiny::p("View your sample data and metadata below. Click the \"Export Data\" button to download this MDMAPR file. You can upload this file in the \"Load Saved Data\" panel above to reanalyze with MDMAPR."),
                column(6, downloadButton(outputId="downloadZipData",label="Download MDMAPR Data File")),
                br(),
                DT::dataTableOutput("dataexport")))

  )

}
