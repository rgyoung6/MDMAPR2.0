
mappingDashboard <- function() {
  shinydashboard::tabItem(tabName = "dashboard",shiny::h1(shiny::strong("Mapping and Filtering")),
    shiny::p("Perform geospatial analysis based on collection locations for run qPCR samples.",
             style = "font-size:16px;"),
    tabBox(id = "map_filter_table_tabbox",width = 12,
      tabPanel("Mapping", shinycssloaders::withSpinner(leaflet::leafletOutput("mymap", height = 700))),
      tabPanel("Data Filtering",
        shinydashboard::box( width = 12,title = "Filter Options",status = "warning",solidHeader = TRUE,collapsible = T,
          shiny::column(2,
            #Radio button to select what type of Cq value you want to based on threshold vaue (User provided or system calculated)
            shiny::radioButtons("thresholdValueButton",
                                "Select Threshold Value to view associated Cq Value:",
                                c("System Calculated Threshold" = "mdmapr", "User Provided Threshold" = "user")),






            #Slider for Cq intensity
            shiny::sliderInput("range", "Select the Cq Intensity",min = 0,
                               max = 40, value=c(0,40),step = 0.10),











            #Slider for date
            shiny::sliderInput("date_input","Select Event Date Range",
                               min =as.Date("2010-01-01", "%Y-%m-%d"),
                               max = as.Date(Sys.Date(),"%Y-%m-%d"),
                               value = range(c(as.Date("2010-01-01", "%Y-%m-%d"),
                                               as.Date(Sys.Date(), "%Y-%m-%d"))),
                               timeFormat = "%Y-%m-%d",step = 1),

            #Dropdown menu for Continent
            shinyWidgets::pickerInput(inputId = "continent_input",
                                    "Continent",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE),
            #Dropdown menu for countries
            shinyWidgets::pickerInput(inputId = "country_input", "Country",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE),
            #Dropdown menu for state or province
            shinyWidgets::pickerInput(inputId = "stateProvince_input",
                                    "State or Province",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE),
            #Dropdown menu for locality
            shinyWidgets::pickerInput(inputId = "locality_input",
                                    "Locality",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE)
          ),shiny::column(4, offset = 1, #End of the shiny column 1
            #Dropdown menu for family type
            shinyWidgets::pickerInput(inputId = "family_input",
                                  "Family",
                                  choices = "None",
                                  selected = "None",
                                  options = list('actions-box' = TRUE),
                                  multiple = TRUE),
            #Dropdown menu for genus type
            shinyWidgets::pickerInput(inputId = "genus_input",
                                  "Genus",
                                  choices = "None",
                                  selected = "None",
                                  options = list('actions-box' = TRUE),
                                  multiple = TRUE),
            #Dropdown menu for species type
            shinyWidgets::pickerInput(inputId = "species_input",
                                  "Species",
                                  choices = "None",
                                  selected = "None",
                                  options = list('actions-box' = TRUE),
                                  multiple = TRUE),
            #Dropdown menu for ct intensity
            shinyWidgets::pickerInput(inputId = "CqIntensity_input",
                                  "Cq Intensity",
                                  choices = "None",
                                  selected = "None",
                                  options = list('actions-box' = TRUE),
                                  multiple = TRUE),
            #Dropdown menu for Machine type
            shinyWidgets::pickerInput(inputId = "machine_input",
                                  "Machine Type",
                                  choices = "None",
                                  selected = "None",
                                  options = list('actions-box' = TRUE),
                                  multiple = TRUE)
          ),shiny::column(4, offset = 1,
            #Dropdown menu for Target gene
            shinyWidgets::pickerInput(inputId = "targetGene_input",
                                    "Target Gene",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE),
            #Dropdown menu for project ID
            shinyWidgets::pickerInput(inputId = "projectID_input", "Project",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE),
            #Dropdown menu for assay
            shinyWidgets::pickerInput(inputId = "assay_input", "Assay",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE),
            #Dropdown menu for establishment means
            shinyWidgets::pickerInput(inputId = "establishmentMeans_input",
                                    "Establishment Means",
                                    choices = "None",
                                    selected = "None",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE)
          )
        )),#End of top box
        tabPanel("Mapped Data Table", DT::dataTableOutput("mapping_data")
      )#Tab panel
    )#Tab Box
  )#Tab Item
}
