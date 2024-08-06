# Define UI for application ---------------------------
shinyAppUI <- shinydashboard::dashboardPage(
  #Skin color of app
  skin = "blue",

  ##Title content
  shinydashboard::dashboardHeader(title ="MDMAPR 2.0"),

  ##Sidebar content
  shinydashboard::dashboardSidebar(width=230,
    shinydashboard::sidebarMenu(id = "sidebarMenu",
      #To allow app to use functions from shinyjs package
      shinyjs::useShinyjs(),
      #Icons for sidebar were obtained from https://fontawesome.com/icons?from=io
      shinydashboard::menuItem("Welcome", tabName = "welcomepage", icon = shiny::icon("door-open")),
      shinydashboard::menuItem("Data Import/Export", tabName = "dataImport", icon = shiny::icon("database")),
      shinydashboard::menuItem("Mapping Dashboard", tabName = "mapDashboard", icon = shiny::icon("map")),
      shinydashboard::menuItem("Individual qPCR Result Analysis", tabName = "qPCRDataOverviewPage", icon = shiny::icon("calculator")),
      shinydashboard::menuItem("Standard Curve Analysis", tabName = "stdCurve", icon = shiny::icon("calculator"))
    )
  ),

  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      ################# Welcome page ###########################################
      shinydashboard::tabItem(tabName = "welcomepage",
        shiny::h1(shiny::strong("Resources")),
        shiny:: br(),
        shiny::fluidRow(shinydashboard::box(
          title = shiny::p("Vignettes", style = "font-size:16px;"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          shiny::p("Information on the vignettes will go here."))
        ),
        shiny::fluidRow(shinydashboard::box(
          title = shiny::p("Metadata Template", style = "font-size:16px;"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          shiny::p("To learn how to fill in the MDMAPR 2.0 Metadata Template please visit the ",
                   shiny::tags$a(href="https://github.com/AlkaBenawra/MDMAPR", "MDMAPR 2.0 GitHub page."),  "The wiki page contains instructions on how to fill in the Metadata template excel file and has a complete guide with descriptions for each field in the metadata template.",  style = "font-size:16px;" ),
          shiny::downloadLink("downloadTemplate",
                              shiny::p("Click Here to Download the Metadata Template",
                                       style = "font-size:16px; color:#F4412E; text-decoration: underline;")
          ))
        ),
        shiny::fluidRow(shinydashboard::box(
          title = shiny::p("How are the System Calculated Threshold values determined?",
                           style = "font-size:16px;"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          shiny::p("The Second Derivative Method is used to calculate the System Calculated Threshold values.", style = "font-size:16px;"))
        ),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::p("What formula is used to calculate the Cq values?",
                                               style = "font-size:16px;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            shiny::p("Cq values are determined by using the point at which each sample crosses the threshold value.", style = "font-size:16px;")
          )
        )
      ), # end of tab item
      ################# Data Import ############################################
      shinydashboard::tabItem(tabName = "dataImport",shiny::h1(shiny::strong("Data Import and Export")),
        shiny::br(),
        shinydashboard::box(title = "File Selection", status = "info",width = 12, solidHeader = TRUE,
          shiny::column(width = 12,
            shiny::p("There are three options for file selection before submission:"),
            shiny::tags$ul(
              shiny::tags$li("Select an RDML file with an associated MDMAPR data file."),
              shiny::tags$li("Only select a MDMAPR file with complete data."),
              shiny::tags$li("Only select an RDML file (as long as associated data are already loaded in this instance of MDMAPR).")
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 12,
                #Upload qPCR experimental fluorescence file
                shiny::fluidRow(
                  #Data file upload
                  shiny::column(1,shinyFiles::shinyFilesButton("qpcr_file_button", "RDML File",title = "RDML File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL)),
                  shiny::column(5,shiny::textOutput("qpcr_file_out")),
                  #Upload metadata file
                  shiny::column(1,shinyFiles::shinyFilesButton("metadata_file_button", "MDMAPR File",title = "MDMAPR File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL)),
                  shiny::column(5,shiny::textOutput("metadata_file_out"))
                )
              )
            )
          )#Closing the full column width
        ),#closing the full width first box
        shinydashboard::box(title = "Actions",status = "info",width = 12, solidHeader = TRUE,
          shiny::column(width = 12,
            shiny::p("There are three options:"),
            shiny::tags$ul(
              shiny::tags$li("1. Submit the selected files."),
              shiny::tags$li("2. Reset the file selections."),
              shiny::tags$li("3. Download the data present in this instance of MDMAPR (output is a .tsv MDMAPR file format).")
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::actionButton("submitImport","Submit File Selection", icon = shiny::icon("play-circle"))),
              shiny::column(3, shiny::actionButton("resetDataImport","Reset File Selection", icon = shiny::icon("refresh"))),
              shiny::column(3, shiny::textOutput("data_present")),
              shiny::column(3, shiny::actionButton("downloadMDMAPRTable","Data (MDMAPR File)", icon = shiny::icon("download")))
            )
          )
        )#Closing the full width second box
      ),#Closing the tab item
      ################# Mapping Dashboard ######################################
      shinydashboard::tabItem(tabName = "mapDashboard",shiny::h1(shiny::strong("Mapping and Filtering")),
        shiny::p("Perform geospatial analysis based on collection locations for run qPCR samples.",
          style = "font-size:16px;"),
        shinydashboard::tabBox(id = "map_filter_table_tabbox",width = 12,
          tabsetPanel(id = "mapTabs",
            shiny::tabPanel("Mapping", shinycssloaders::withSpinner(leaflet::leafletOutput("mymap", height = 700))),
            shiny::tabPanel("Data Filtering",
            shinydashboard::box( width = 12,title = "Filter Options",status = "warning",solidHeader = TRUE,collapsible = T,
              shiny::column(3,
                #Dropdown menu for project ID
                shinyWidgets::pickerInput(inputId = "projectID_input", "Project",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE),
                #Dropdown menu for Machine type
                shinyWidgets::pickerInput(inputId = "machine_input",
                  "Platform",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE),
                #Dropdown menu for Target gene
                shinyWidgets::pickerInput(inputId = "targetGene_input",
                  "Target Gene",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE),
                #Dropdown menu for assay
                shinyWidgets::pickerInput(inputId = "assay_input", "Assay",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE)
              ),shiny::column(3, offset = 1, #End of the shiny column 1
                #Radio button to select what type of Cq value you want to based on threshold value (User provided or system calculated)
                shiny::radioButtons("mappedValueButton",
                  "Select Threshold Value to view associated Cq Value:",
                  c("User Cq" = "resultUserProvCq",
                    "User Copy" = "resultUserCopyNum",
                    "User Thres" = "resultUserProvThres",
                    "MDMAPR Cq" = "mdmaprCq",
                    "MDMAPR Copy" = "mdmaprCopyNum",
                    "MDMAPR Thres" = "mdmaprThres"
                  )
                ),
                shiny::checkboxInput("mappedValIncludeEmpty", "Include empty, NA, and 'Unable to Compute' values", value = TRUE),
                #Dropdown menu for genus
                shinyWidgets::pickerInput(inputId = "resultSampleType_input",
                  "Sample Type",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE)
              ),shiny::column(3, offset = 1,
                #Slider for date
                shiny::sliderInput("date_input","Select Event Date Range",
                  min =as.Date("1900-01-01", "%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value = range(c(as.Date("1900-01-01", "%Y-%m-%d"),
                                 as.Date(Sys.Date(), "%Y-%m-%d"))),
                  timeFormat = "%Y-%m-%d",step = 1),
                shiny::checkboxInput("mappedDateIncludeEmpty", "Include empty and NA values", value = TRUE),
                #Dropdown menu for genus
                shinyWidgets::pickerInput(inputId = "genus_input",
                  "Genus",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE),
                #Dropdown menu for species
                shinyWidgets::pickerInput(inputId = "species_input",
                  "Species",
                  choices = "None",
                  selected = "None",
                  options = list('actions-box' = TRUE),
                  multiple = TRUE),
                #Data file upload
                shiny::fluidRow(
                  shiny::column(6,shiny::actionButton("updateFilterMappingButton", "Update Filtering Options", icon = shiny::icon("play"))),
                  shiny::column(6,shiny::actionButton("resetFilterMappingButton", "Reset Filtering Options", icon = shiny::icon("refresh")))
                )
              )
            )#End of top box
          ),
          shiny::tabPanel("Mapped Data Table",
            #Download Mapped data
            shiny::p(shiny::strong("Mapped Markers Metadata"),
              style = "font-size:25px"),
            shiny::actionButton("downloadFilteredData","Download Filtered Data File", icon = shiny::icon("download")),
            DT::dataTableOutput("mapping_data")
            )#Tab panel
          )#Tab Box
        )#TabsetPanel Closing
      ),#Tab Item

      ################# qPCR Data Overview page ################################
      shinydashboard::tabItem(tabName = "qPCRDataOverviewPage",
        shiny::h1(shiny::strong("qPCR Data Overview")),
        #Dropdown menu to select standard curve to view
        shiny::fluidRow(
          shinydashboard::box(id = "tabset1",
            width = 3,
            #Tab1
            shiny::tabPanel("Data",
              shiny::column(12,
                shiny::p("To visualize individual qPCR results directly select the loaded reaction of interest, or use the filters to assist you in finding the reaction of interest.",  style = "font-size:16px;"),
                # div(style = "border: 1px solid black; padding: 10px; margin-bottom: 10px;",
                #
                # ),
                #Dropdown menu for projectID
                shinyWidgets::pickerInput(inputId = "project_qPCROverview",
                  "Project",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for siteID
                shinyWidgets::pickerInput(inputId = "site_qPCROverview",
                  "Site",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for stationID
                shinyWidgets::pickerInput(inputId = "station_qPCROverview",
                  "Station",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for replicateID
                shinyWidgets::pickerInput(inputId = "replicate_qPCROverview",
                  "Replicate",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for extractID
                shinyWidgets::pickerInput(inputId = "extractID_qPCROverview",
                  "Extract",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for assayID
                shinyWidgets::pickerInput(inputId = "assay_qPCROverview",
                  "Assay",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for resultRunID
                shinyWidgets::pickerInput(inputId = "runID_qPCROverview",
                  "Run",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                #Dropdown menu for resultReactID
                shinyWidgets::pickerInput(inputId = "reactionID_qPCROverview",
                  "Reaction",
                  choices = "None",
                  selected = "None",
                  multiple = TRUE),
                shiny::column(6,shiny::actionButton("updateqPCRselections", "Update qPCR Selections", icon = shiny::icon("play"))),
                shiny::column(3, shiny::actionButton("resetqPCRselections","Reset qPCR Selections", icon = shiny::icon("refresh")))
              )#End fo the column
            )#End of the Data tabPanel
          ),#End of the Box
          shinydashboard::box(title = shiny::strong("Amplification Plot"),id = "ampBox",
            id = "data_analysis_box",
            height = 1000,
            width = 9,
            shiny::tabPanel("ampPlot", #shiny::h4(shiny::strong("Amplification Plot")),
              shiny::tags$hr(),  # Adds a horizontal line above the fluidRow
              fluidRow(
                column(6,
                  # shiny::h4("Select Data Groups"),
                  shiny::checkboxGroupInput("groups", "Show Groups:",
                    choices = list(
                      "MDMAPR - Blue" = "mdmapr",
                      "Users - Red" = "resultUserProv"
                    ),
                    selected = c("mdmapr", "resultUserProv"))
                ),
                column(6,
                  # shiny::h4("Select Elements"),
                  shiny::checkboxGroupInput("elements", "Show Elements:",
                    choices = list(
                      "Threshold - Dashed Horz." = "thres",
                      "LOD - Circles" = "lod",
                      "LOQ - Triangles" = "loq",
                      "Cq - Squares" = "cq",
                      "Log Linear Area - Shaded" = "loglinear"
                    ),
                    selected = c("thres", "lod", "loq","cq", "loglinear"),
                    inline = TRUE)
                )),
              plotly::plotlyOutput("qPCROverviewPlot")
            )#end of tab panel
          )
        )
      ),

      ################# Standard Curve Page ######################################
      shinydashboard::tabItem(tabName = "stdCurve",
        shiny::h1(shiny::strong("Standard Curve Analysis")),
        shiny::fluidRow(
          shinydashboard::box(id = "tabset1",width = 3,height = "800px",
            shiny::fluidRow(shiny::column(12,
              # shiny::br(),
              # shiny::p("To analyze standard curve data, upload a qPCR standard curve
              #           fluorescence file, and a filled in metadata file.",
              #           style = "font-size:16px;"),
              # shiny::numericInput(inputId = "LOQthres",label="LOQ Coefficient of Variation Threshold",
              #                     value=0.35,min=0,max=0.99, step = 0.01),
              # shiny::br(),
              #Dropdown menu for project - projectID
              shinyWidgets::pickerInput(inputId = "SC_project_input","Project",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
#              shiny::br(),
              #Dropdown menu for assay - assayID
              shinyWidgets::pickerInput(inputId = "SC_assay_input", "Assay",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
#              shiny::br(),
              #Dropdown menu for run type - resultRunID
              shinyWidgets::pickerInput(inputId = "SC_run_input", "Run",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
#              shiny::br(),
              #Dropdown menu for machine type - resultPlatform
              shinyWidgets::pickerInput(inputId = "SC_platform_input", "Platform",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
              #Dropdown menu for machine type - resultMachineID
              shinyWidgets::pickerInput(inputId = "SC_machine_input", "Machine",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
#              shiny::br(),
              #Wells in the standard Curve - resultStdCurveID
              shinyWidgets::pickerInput(inputId = "SC_curve_input",label = "Standard Curve",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
              #Wells in the Wells Curve - resultReactID
              shinyWidgets::pickerInput(inputId = "SC_react_input",label = "Select specific reaction to remove.",
                choices = "None",
                selected = "None",
                options = list('actions-box' = TRUE),
                multiple = TRUE),
              shiny::p(shiny::strong("Action Buttons")),
              shiny::fluidRow(
                shiny::column(5,
                  shiny::actionButton("SC_Update","Update Selections")
                ),
                shiny::column(5,
                  shiny::actionButton("SC_reset","Reset Selections"),
                )
              ),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(5,
                  shiny::actionButton("SC_recalib","Calibrate Curve")
                ),
                shiny::column(5,
                  shiny::actionButton("SC_Remove","Remove Selected From Curve")
                )
              )
              # shinydashboard::box(width = NULL, solidHeader = TRUE,collapsible = TRUE, collapsed = F,
              #   title = "LOD Calculation Messages",
              #   status = "warning",shiny::column(12, shiny::textOutput("text")))
            ))# end of fluid row
          ),#end of box
          #Standard curve plot
          shinydashboard::box(title = shiny::strong("Standard Curve Analysis"),id = "data_analysis_box",
          # shinydashboard::tabBox(title = shiny::strong("Standard Curve Analysis"),id = "data_analysis_box",
             height = "1000px",width = 9,
          #   shiny::tabPanel("Standard Curve Data Overview",
          #     shiny::strong("Data from the 'Standard Curve Data Overview' table is visualized in the 'Standard Curve Plot' tab."),
          #       DT::dataTableOutput('SC_overview_table')
          #   ),
            shiny::tabPanel("Standard Curve Plot",
              # shiny::strong("The residual gradient colour scheme depicts how well
              #   the standard curve fits the data points. Yellow points
              #   are best fit by the curve, dark purple points are least
              #   fit, and orange points are in between."),
              shiny::tags$hr(),  # Adds a horizontal line above the fluidRow
              fluidRow(
                column(6,
                  shiny::checkboxGroupInput("groups", "Show Groups:",
                  choices = list(
                    "MDMAPR" = "mdmapr",
                    "Users" = "resultUserProv"
                  ),
                  selected = c("mdmapr", "resultUserProv"), inline = TRUE)
                ),
                column(6,
                       shiny::br(),
                       shiny::p("LOD = Limit of Detection, LOQ = Limit of Quantification")
                )),
              plotly::plotlyOutput("standardCurve_plot")
            )#End of tab panel
          )# end of tab box
        ) #end of fluid row
      )# end of shiny dashboard tab item
      ############################## END of Tab Items ############################
    )
  ) #Closing the Shiny Dash Board Body
) #Closing the ShinyDashBoard Page



