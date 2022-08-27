source('./R/components/dataImport.R')
source('./R/components/mappingDashboard.R')
source('./R/components/stdCurveComponent.R')
source('./R/components/qPCRDataOverviewComponent.R')
source('./R/components/welcomeComponent.R')

dashBoardBodyComponent <- function() {

  shinydashboard::dashboardBody(

    #Style tag for website name, located in left top corner of page
    shiny::tags$head(shiny::tags$style(shiny::HTML('
      .main-header .logo {
        font-family: Verdana, Geneva, sans-serif;
        font-size: 24px;
      }
    '))),

    shinydashboard::tabItems(

      #Mapping Dashboard ---------------------------
      mappingDashboard(),

      # #Data Import ---------------------------
      dataImport(),

      #qPCR Data Overview page ---------------------------
      qPCRDataOverviewComponent(),

      #Data Submission page  ---------------------------
      #dataSubmissionComponent(),

      #Welcome page ---------------------------
      welcomeComponent(),

      #Standard Curve Page ---------------------------
      stdCurve()

      #Get Started page ---------------------------
      #dataModelling()

      #Std Curve Componenet ---------------------------
      #stdCurve()
    )
  )
}
