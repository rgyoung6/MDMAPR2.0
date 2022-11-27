#' @import shinydashboard
#' @import shiny
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


welcomeComponent <- function() {
  shinydashboard::tabItem(tabName = "welcomepage",
          includeHTML("R/components/home.html"),
          shiny::tags$script(src = "plugins/scripts.js"),
          shiny::tags$head(
            shiny::tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
            shiny::tags$link(rel = "icon",
                      type = "image/png",
                      href = system.file("www/logo_icon.png", "MDMAPR2.0"))
          ),
          shiny::br(),
          box(title = "Vignettes",
              status = "primary", solidHeader = TRUE,
              width = 12,
              fluidRow(
                #shiny::column(11,fluidPage(includeMarkdown("./R/MDMAPR_Vignettes.Rmd")))
              )),
          shiny::br(),

          shiny::h1(shiny::strong("FAQs")),

          shiny:: br(),

          fluidRow(box(
            title = shiny::p("Metadata Template",
                      style = "font-size:16px;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            shiny::p("To learn how to fill in the MDMAPR 2.0 Metadata Template please visit the ",
              shiny::tags$a(href="https://github.com/AlkaBenawra/MDMAPR", "MDMAPR 2.0 GitHub page."),  "The wiki page contains instructions on how to fill in the Metadata template excel file and has a complete guide with descriptions for each field in the metadata template.",  style = "font-size:16px;" ),

            downloadLink("downloadTemplate",
                         shiny::p("Click Here to Download the Metadata Template",
                           style = "font-size:16px;
                                             color:#F4412E;
                                              text-decoration: underline;"))
          )),

          fluidRow(box(
            title = shiny::p("How are target copy numbers calculated?",
                      style = "font-size:16px;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            shiny::p("If standard curve fluorescence values and metadata are provided, the system will calculate the second derivative threshold and Cq Value. These are related to the DNA copy number value by a linear model and used to estimate copy number in the experimental samples.")
          )),

          fluidRow(box(
            title = shiny::p("How are the System Calculated Threshold values determined?",
                      style = "font-size:16px;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            shiny::p("The Second Derivative Method is used to calculate the System Calculated Threshold values.", style = "font-size:16px;"))),

          fluidRow(
            box(title = shiny::p("What formula is used to calculate the Cq values?",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                shiny::p("The function th.cyc() from the R package chipPCR is used to calculate the Cq values.", style = "font-size:16px;")))
  ) # end of tab item

}

