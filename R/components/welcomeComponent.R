
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

