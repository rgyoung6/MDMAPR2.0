
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

                       #Upload metadata file
                       shiny::fileInput("metadata_file", "Upload Filled MDMAPR Metadata File (tsv)", width="500px",
                                 multiple = FALSE,
                                 accept = c(".tsv")),
                       shiny::fluidRow(shiny::column(12,textOutput("error_msg"))),
                       shiny::fluidRow(
                         shiny::column(6, actionButton("submit",
                                                "Submit  Files")),
                         shiny::column(6, actionButton("resetDataImport",
                                                "Reset Files")))
                   )),# end of upload data box and column

            shiny::column(width = 6,
                   shinydashboard::box(title = span( icon("file-export"), "Data Export"),
                                       status="info", solidHeader=TRUE, width=12,collapsible = T,collapsed = F,
                      shiny::p("Download all data that are loaded into this instance of MDMAPR in either a large data table or for the raw absorbance data in RDML format."),
                      shiny::fluidRow(
                        shiny::column(6, downloadButton(outputId="downloadMDMAPRTable",label="MDMAPR metadata file")),
                        shiny::column(6, downloadButton(outputId="downloadRDMLFile",label="RDML file")))
                   ) # end of export data box
      )#End of right column
    )#End of establishing rows
  )# End of tab element
}
