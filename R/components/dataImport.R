
dataImport <- function() {
  shinydashboard::tabItem(tabName = "dataImport",shiny::h1(shiny::strong("Data Import and Export")),
    br(),
    shinydashboard::box(title = "File Selection", status = "info",width = 12, solidHeader = TRUE,
      shiny::column(width = 12,
        shiny::p("There are three options for file selection before submission:
                 1. Select an RDML file with an associated MDMAPR data file.
                 2. Only select a MDMAPR file with complete data.
                 3. Only select an RDML file (as long as associated data are already loaded in this instance of MDMAPR)."),
        br(),
        shiny::fluidRow(
          shiny::column(width = 12,
            #Upload qPCR experimental fluorescence file
            shiny::fluidRow(
              #Data file upload
              column(3,actionButton("qpcr_file_button", "RDML", icon = icon("magnifying-glass"))),
              column(3,textOutput("qpcr_file_out")),
              #Upload metadata file
              column(3,actionButton("metadata_file_button","MDMAPR", icon = icon("magnifying-glass"))),
              column(3,textOutput("metadata_file_out"))
            )
          )
        )
      )#Closing the full column width
    ),#closing the full width first box
    shinydashboard::box(title = "Actions",status = "info",width = 12, solidHeader = TRUE,
      shiny::column(width = 12,
        shiny::p("There are three options:
                 1. Submit the selected files.
                 2. Reset the file selections.
                 3. Download the data present in this instance of MDMAPR (output is a .tsv MDMAPR file format."),
        br(),
        shiny::fluidRow(
          shiny::column(3, actionButton("submit","Submit File Selection", icon = icon("play-circle"))),
          shiny::column(3, actionButton("resetDataImport","Reset File Selection", icon = icon("refresh"))),
          column(3,textOutput("data_present")),
          shiny::column(3, downloadButton(outputId="downloadMDMAPRTable",label="Data (MDMAPR File)"))
        )
      )
    )#Closing the full width second box
  )#Closing the tab item
}#Closing the function
