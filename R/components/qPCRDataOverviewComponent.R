
#qPCR Data Overview page ---------------------------

qPCRDataOverviewComponent <- function() {
  shinydashboard::tabItem(tabName = "qPCRDataOverviewPage",

                          shiny::h1(shiny::strong("qPCR Data Overview")),

                          shiny::p("Analyze individual well samples for a qPCR run.",
            style = "font-size:16px;"),

          br(),

          #Dropdown menu to select standard curve to view
          fluidRow(

            box(id = "tabset1",
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
