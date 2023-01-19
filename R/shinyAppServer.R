#' @import shinyalert
#' @import shinydashboard
#' @import shiny
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @import leaflet
#' @import here
#' @import leaflet.extras
#' @importFrom shinydashboard box
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
#' @import RDML
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
#' @import shinyalert


shinyAppServer <- function(input, output, session) {

############## Initialize variables #########################################

print("HERE RIGHT AT THE VERY BEGINNING")

  #dataImport file location reactive values
  qpcr_file <- reactiveValues(data = NA)
  metadata_file <- reactiveValues(data = NA)

  #Reactive value once updated will start the mapping render
  uploaded_data <- reactiveValues(value="Initial")

  #Reactive value once updated will start the data processing
  upload_data_check <- reactiveValues(value = "Initial")

  #Initialize what the output filed will say on the dataImport page
  output$qpcr_file_out <- renderText({as.character("No data selected")})
  output$metadata_file_out <- renderText({as.character("No data selected")})
  output$data_present <- renderText({ as.character("No Loaded Data" )})

  #Reactive values to hold the read in metadata and RDML data from one
  #observeEvent (ObserveSubmit) to another (ObserveUpload_data)check)
  formatted_metadata <- reactiveValues(value = "Initial")
  fdata<-reactiveValues(value = "Initial")
  cq_column <- reactiveValues(value="Initial")


  #Reactive value to indicate that the user wants to calculate data and not simply view data
  complete_calc<-reactiveValues(value = "YES")

print("Here 1")

###################### Data Import/Export Buttons and Pop up messages ##########

  # Show modal when button is clicked.
  observeEvent(input$qpcr_file_button, {
    tryCatch(
      expr = {
        qpcr_file$data <- file.choose()
        output$qpcr_file_out <- renderText({as.character(qpcr_file$data)})
      },
      error = function(e){
        print("Error - qpcr_file_button choose file cancelled")
        qpcr_file$data <- NA
      },
      warning = function(w){
        print("Warning - qpcr_file_button choose file cancelled")
        qpcr_file$data <- NA
      }
    )
  },ignoreInit = TRUE)

print("Here 2")

  # Show modal when button is clicked.
  observeEvent(input$metadata_file_button, {
    tryCatch(
      expr = {
        metadata_file$data <- file.choose()
        output$metadata_file_out <- renderText({as.character(metadata_file$data)})
      },
      error = function(e){
        print("Error - qpcr_file_button choose file cancelled")
        metadata_file$data <- NA
      },
      warning = function(w){
        print("Warning - qpcr_file_button choose file cancelled")
        metadata_file$data <- NA
      },
      finally = {
      }
    )
  },ignoreInit = TRUE)

print("Here 3")

  #Data reset button
  observeEvent(input$resetDataImport, {
    output$qpcr_file_out <- renderText({as.character("No data selected")})
print("Reset 1")
    output$metadata_file_out <- renderText({as.character("No data selected")})
print("Reset 2")
    qpcr_file$data <- NA
print("Reset 3")
    metadata_file$data <- NA
print("Reset 4")
uploaded_data$value <- "Initial"
print("Reset 5")
    upload_data_check$value <- "Initial"
print("Reset 6")
    formatted_metadata$value <- "Initial"
print("Reset 7")
    fdata$value <- "Initial"
print("Reset 8")
    complete_calc$value <- "YES"
print("Reset 9")


print("Here as data_present set to No Loaded Data in reset - line 168")

    output$data_present <- renderText({ as.character("No Loaded Data" )})
    showModal(modalDialog(
      title = "Cleared Data",
      "Data have been cleared from this MDMAPR instance!"
    ))

  },ignoreInit = TRUE)

print("Here 4")

  # A modal dialog checking to see there is data present in the filtered$value
  dataModal <- function(failed = FALSE) {
    modalDialog(
      ('Please select if you would like to overwrite the data or combine the data...'),
      if (failed)
        div(tags$b("Invalid selection please try again!", style = "color: red;")),
      footer = tagList(
        actionButton("overwrite_data", "Overwrite"),
        actionButton("combo_data", "Combine")
      )
    )
  }

print("Here 5")

  #This observeEvent checks the response when a set of data is submitted where
  #one is already loaded. This indicates the overwrite response
  observeEvent(input$overwrite_data, {
    upload_data_check$value <- "Overwrite"
    removeModal()
  },ignoreInit = TRUE)

print("Here 6")

  #This observeEvent checks the response when a set of data is submitted where
  #one is already loaded. This indicates the combine response
  observeEvent(input$combo_data, {
    upload_data_check$value <- "Combine"
    removeModal()
  },ignoreInit = TRUE)

print("Here 7")

  # A modal dialog checking to see there is data present in the filtered$value
  complete_calc_Modal <- function(failed = FALSE) {
    modalDialog(
      ("Only a MDMPAR data file was submitted with no previously loaded data. Would you like to generate calculated values for these data and then visualize or just visualize? Please note that only records with complete visualization data (including absorbance data) will be used."),
      if (failed)
        div(tags$b("Invalid selection please try again!", style = "color: red;")),
      footer = tagList(
        actionButton("calc_and_visualize", "Calculate and Visualize"),
        actionButton("visualize", "Visualize")
      )
    )
  }

print("Here 8")

  #This observeEvent checks the response when asked if the data should be used to complete calculations
  observeEvent(input$calc_and_visualize, {
    complete_calc$value <- "YES"
    upload_data_check$value <- "New"
    removeModal()
  },ignoreInit = TRUE)
  #This observeEvent checks the response when asked if the data should be used to complete calculations
  observeEvent(input$visualize, {
    complete_calc$value <- "NO"
    upload_data_check$value <- "New"
    removeModal()
  },ignoreInit = TRUE)

print("Here 9")

####################### Data Export Download Button ###########################

  #Download button for downloading CSV of filtered mapping data
  output$downloadMDMAPRTable <- downloadHandler(
    filename = function() {
     paste0(format(Sys.time(), "%Y_%m_%d_%H%M"), "_MDMAPR_Data.tsv")
    },
    content = function(file) {
      write.csv(as.data.frame(filtered$value[2:ncol(uploaded_data$value)]), file,  row.names=FALSE)
    }
  )

print("Here 10")

############## Data Processing Upon Hitting Submit ############################

  observeEvent(input$submit, {

#The below content is checking the files submitted adhere to MDMAPR standards
    if(is.null(qpcr_file$data) & is.null(metadata_file$data)){
      showModal(modalDialog(
        title = "Missing Data",
        "There are no files present, please select data files and resubmit."
      ))
    }else if(is.na(qpcr_file$data) & is.na(metadata_file$data)){
      showModal(modalDialog(
        title = "Missing Data",
        "There are no files present, please select data files and resubmit."
      ))
    }else if(is.na(qpcr_file$data) & !is.na(metadata_file$data)){
      #Check to make sure that the file extension is correct
      if(grepl("\\.[Tt][Ss][Vv]$", metadata_file$data)){
        #Loading in the MDMAPR data file
        tryCatch(
          expr = {
            #Load in the data to the formatted_metadata variable and check format
            formatted_metadata$value<-read.table(metadata_file$data, header=TRUE, sep="\t", dec=".")

print("Here as data_present set to Loaded Data Only metadata loaded - line 275")

            output$data_present <- renderText({ as.character("Loaded Data" )})
          },
          error = function(e){
            print("Error - loading the MDMAPR data file, please check the file and try again.")
            showModal(modalDialog(
              title = "Incorrect File Type",
              "Incorrect file typ, please select a properly formatted '.tsv' file and resubmit."
            ))
            metadata_file$data <- NA
          },
          warning = function(w){
            print("Error - loading the MDMAPR data file, please check the file and try again.")
            showModal(modalDialog(
              title = "Incorrect File Type",
              "Incorrect file typ, please select a properly formatted '.tsv' file and resubmit."
            ))
            metadata_file$data <- NA
          }
        )#Closing trycatch
        suppressWarnings(if (uploaded_data$value == "Initial"){
          showModal(complete_calc_Modal())
        }else{
          showModal(dataModal())
        })
      }else{#The file extension was incorrect
        showModal(modalDialog(
          title = "Incorrect File Type",
          "Incorrect file typ, please select a properly formatted '.tsv' file and resubmit."
        ))
      }#End of if-else right extension
    }else if(!is.na(qpcr_file$data) & is.na(metadata_file$data)){
      if(grepl("\\.[Rr][Dd][Mm][Ll]$", qpcr_file$data)){
        tryCatch(
          expr = {
            #Load in the RDML data
            raw_data <- RDML$new(filename = qpcr_file$data)
            #pull all the fluorescence data
            fdata$value <- as.data.frame(raw_data$GetFData(long.table = T))
          },
          error = function(e){
            print("Error - loading the MDMAPR data file, please check the file and try again.")
            showModal(modalDialog(
              title = "Incorrect File Type",
              "Incorrect file typ, please select a properly formatted '.rdml' file and resubmit."
            ))
            qpcr_file$data <- NA
          },
          warning = function(w){
            print("Error - loading the MDMAPR data file, please check the file and try again.")
            showModal(modalDialog(
              title = "Incorrect File Type",
              "Incorrect file typ, please select a properly formatted '.rdml' file and resubmit."
            ))
            qpcr_file$data <- NA
          }
        )#Closing trycatch
        suppressWarnings(if (uploaded_data$value == "Initial"){
          showModal(modalDialog(
            title = "No MDMAPR data present",
            "No MDMAPR data are loaded and no MDMAPR data file was submitted. Please resubmit the RDML data file with an associated MDMAPR data file."
          ))
        }else{
          showModal(dataModal())
        })
      }else{#The file extension was incorrect
        showModal(modalDialog(
          title = "Incorrect File Type",
          "Incorrect file typ, please select a properly formatted '.rdml' file and resubmit."
        ))
      }#End of if-else right extension
    }else if(!is.na(qpcr_file$data) & !is.na(metadata_file$data)){
      if(grepl("\\.[Rr][Dd][Mm][Ll]$", qpcr_file$data) & grepl("\\.[Tt][Ss][Vv]$", metadata_file$data)){
        #Loading in the MDMAPR data file
        tryCatch(
          expr = {
            #Load in the data to the formatted_metadata variable
            formatted_metadata$value<-read.table(metadata_file$data, header=TRUE, sep="\t", dec=".")
            #Load in the RDML data
            raw_data <- RDML$new(filename = qpcr_file$data)
            #pull all the fluorescence data
            fdata$value <- as.data.frame(raw_data$GetFData(long.table = T))

print("Here as data_present set to Loaded Data Both RDML and MDMAPR loaded - line 359")

            output$data_present <- renderText({ as.character("Loaded Data" )})
          },
          error = function(e){
            print("Error - loading the MDMAPR data file, please check the file and try again.")
            metadata_file$data <- NA
          },
          warning = function(w){
            print("Error - loading the MDMAPR data file, please check the file and try again.")
            metadata_file$data <- NA
          }
        )#Closing trycatch
        suppressWarnings(if (uploaded_data$value == "Initial"){
          upload_data_check$value <- "New"
        }else{
          showModal(dataModal())
        })
      }else{#The file extension was incorrect
        showModal(modalDialog(
          title = "Incorrect File Types",
          "Incorrect file type(s), please select a properly formatted '.rdml' and/or '.tsv' file(s) and resubmit."
        ))
      }#End of checking file name
    }#End of checking files
  },ignoreInit = TRUE)# end of the observeEvent Input Submit

################# process data observeEvent ####################################

print("Here 11A")

  observeEvent(upload_data_check$value, {

    suppressWarnings(if(upload_data_check$value != "Initial"){

      print(paste0("At the beginning of the process_data function."))

      withProgress(message = 'Processing data', value = 0, {

        # 1. Format the metadata
        suppressWarnings(if(formatted_metadata$value =="Initial"){
          formatted_metadata$value<- as.data.frame(uploaded_data$value)
        }else{
          #Add a uniqueID to the beginning of the dataframe including projectID resultRunID resultReactID
          #Which will correspond to the exp.id run.id react.id from RDML
          built_unique_ID<-paste0(formatted_metadata$value$projectID,"_",formatted_metadata$value$resultRunID,"_", formatted_metadata$value$resultReactID)
          formatted_metadata$value<-cbind(built_unique_ID, formatted_metadata$value)
        })
        # Increment the progress bar, and update the detail text.
        incProgress(1/8, detail = paste("Formatting Metadata"))

        # 2. Read in the RDML file
        print("Processing RDML file")
        #If there was RDML data then add these records to the
        suppressWarnings(if(fdata$value!="Initial"){
          raw_multiplex_data <- process_Multiplexed_RDML(fdata$value)
        }else{
          #Create a raw_multiplex_data variable for all data in the MDMAPR file submitted
          #Get the total number of cycles used for the standard curve first select only cycle columns
          raw_multiplex_data <- formatted_metadata$value[,grep("Cycle", names(formatted_metadata$value), value = TRUE)]
          #Remove columns with only NA values
          raw_multiplex_data <- raw_multiplex_data[,colSums(is.na(raw_multiplex_data))<nrow(raw_multiplex_data)]
          if(ncol(raw_multiplex_data)==0){
            complete_calc$value <- "ERROR"
          }else{
            raw_multiplex_data <- cbind(formatted_metadata$value$built_unique_ID, raw_multiplex_data)
          }
        })

        # Increment the progress bar, and update the detail text.
        incProgress(1/8, detail = paste("Processing RDML"))
        if(complete_calc$value == "YES"){

          # 3. Calculate the second derivative threshold
          print("Calculating MDMAPR threshold")
          # confirm built_unique_ID is the row name
          row.names(raw_multiplex_data)<- as.character(raw_multiplex_data$built_unique_ID)

          # remove the column that contains the built_unique_ID information
          raw_multiplex_data <- raw_multiplex_data[,-1]

          # calculate threshold
          raw_multiplex_data <- calculate_second_deriv_threshold(raw_multiplex_data)

          # Add the user provided threshold value
          raw_multiplex_data <- merge(as.data.frame(raw_multiplex_data), formatted_metadata$value[ , c("resultUserProvThres", "built_unique_ID")], by="built_unique_ID")

          # Increment the progress bar, and update the detail text.
          incProgress(1/8, detail = paste("Calculating Threshold"))

          # 4. Calculate the Cq value using the threshold
          print("Calculating MDMAPR threshold Cq")
          raw_multiplex_data <- add_Cq(raw_multiplex_data, "mdmaprThres", "mdmaprCq")

          # Increment the progress bar, and update the detail text.
          incProgress(1/8, detail = paste("Calculating MDMAPR Cq"))

          # 5. If the user has threshold values provided, calculate the Cq value with that threshold
          print("Calculating user threshold Cq")
          raw_multiplex_data <- add_Cq(raw_multiplex_data, "resultUserProvThres", "mdmaprCqwUserThres")

          #Remove user supplied threshold as it already exists in the dataframe
          raw_multiplex_data<-raw_multiplex_data[ , !names(raw_multiplex_data) %in% c("resultUserProvThres")]

          # Increment the progress bar, and update the detail text.
          incProgress(1/8, detail = paste("Calculating MDMAPR Cq"))

          # 6. Merge all the data
          print("Merge all inputted and calculated data")
          #First get all of the RDML and metadata unique ID's
          total_built_unique_ID<-unique(formatted_metadata$value$built_unique_ID)

          #Error checks - print error to console
          if (length(formatted_metadata$value$built_unique_ID)>length(unique(formatted_metadata$value$built_unique_ID))){

            print("Analysis stopped - Duplication of unique key(s) in MDMAPR file with loaded MDMAPR records (MDMAPR Unique key - projectID_resultRunID_resultReactID). Please re-evaluate your files and rerun the program.")
            showModal(modalDialog(
              title = "Error - Duplication of MDMAPR unique key(s)",
              "Analysis stopped - Duplication of unique key(s) present in submitted data please reevaluate the data and resubmit."
            ))
          }else{
            if (length(setdiff(unique(formatted_metadata$value$built_unique_ID), unique(raw_multiplex_data$built_unique_ID)))>0){
              print("Warning - Not all metadata records (as defined by the unique key - projectID_resultRunID_resultReactID) were represented in the uploaded RDML file")
              showModal(modalDialog(
                title = "Warning!",
                "Not all metadata records (as defined by the unique key - projectID_resultRunID_resultReactID) were represented in the uploaded RDML file."
              ))
            }
            if (length(setdiff(unique(raw_multiplex_data$built_unique_ID), unique(formatted_metadata$value$built_unique_ID)))>0){
              print("Warning - Not all RDML records (as defined by the unique key - exp.id_run.id_react.id) were represented in the metadata table")
              showModal(modalDialog(
                title = "Warning!",
                "Not all RDML records (as defined by the unique key - exp.id_run.id_react.id) were represented in the metadata table"
              ))
            }

            #Get all built_unique_ID values for the metadata file
            formatted_metadata_built_unique_ID<-data.frame(formatted_metadata$value$built_unique_ID)
            #Give the one column dataframe a header
            colnames(formatted_metadata_built_unique_ID)<-"built_unique_ID"

            #Merge the Cq calculations on to this dataframe
            raw_multiplex_data <- merge(formatted_metadata_built_unique_ID, raw_multiplex_data, by = "built_unique_ID", all.x = TRUE)

            #Replace the existing columns in the dataframe with these columns
            formatted_metadata$value[names(raw_multiplex_data[-1])] <- raw_multiplex_data[-1]

            # Increment the progress bar, and update the detail text.
            incProgress(1/8, detail = paste("Merging data"))

            # 7. Calculate standard curve data for any records with standard curve absorbance but no calculated data
            print("Calculating standard curve data")

            #Get records that have opt and are therefore designated as standard curve results
            formatted_metadata_SC_Calc<-formatted_metadata$value[formatted_metadata$value$resultSampleType == "opt",]

            #Calculate std curve values using the 35% Klymus method (Could add elow quant to this later)
            #first see if there are records remaining and then see if there are number values in the first cycle
            if(nrow(formatted_metadata_SC_Calc)>0){

              #Add a unique identifier for the sc data sets to the beginning of the data frame as
              #$determined by the projectID, assayID, resultReactID and assayProbeFluorescentTag
              formatted_metadata_SC_Calc<-cbind(paste(formatted_metadata_SC_Calc$projectID,formatted_metadata_SC_Calc$assayID, formatted_metadata_SC_Calc$resultRunID, formatted_metadata_SC_Calc$assayProbeFluorescentTag), formatted_metadata_SC_Calc)

              #Add column name to the newly added column
              colnames(formatted_metadata_SC_Calc)[1] <- "list_of_curves"

              #Get a list of the different standard curves as determined by the projectID and resultReactID and assayProbeFluorescentTag
              list_of_curves<-unique(formatted_metadata_SC_Calc$list_of_curves)

              #Loop through the different standard curves
              for (list_of_curves_counter in 1:length(list_of_curves)){

                #Subset the formatted_metadata_SC_Calc table for the curve for this loop
                formatted_metadata_SC_Calc_loop_curve<-formatted_metadata_SC_Calc[formatted_metadata_SC_Calc$list_of_curves == list_of_curves[list_of_curves_counter],]

                #Get the total number of cycles used for the standard curve first select only cycle columns
                cycles <- formatted_metadata_SC_Calc_loop_curve[,grep("Cycle", names(formatted_metadata_SC_Calc_loop_curve), value = TRUE)]
                #Remove columns with only NA values
                cycles <- cycles[,colSums(is.na(cycles))<nrow(cycles)]
                #count the number of columns to obtain the number of cycles
                cycles <- ncol(cycles)

                #Call the calculate_SC_LOD_LOQ function using the subset data. The function is in
                # the standard_curve_helpers file
                formatted_metadata_SC_Calc_loop_curve<-calculate_SC_LOD_LOQ(formatted_metadata_SC_Calc_loop_curve[, c("list_of_curves", "built_unique_ID", "mdmaprCq", "resultTemplateConcInCopy")], 0.35, cycles)

                #rename the columns from the returned LOD-LOQ calc for the MDMAPR calculated values
                colnames(formatted_metadata_SC_Calc_loop_curve)<-c("built_unique_ID", "list_of_curves",  "mdmaprLOD", "mdmaprLODKlymus35Warn", "mdmaprLOQ")

                #Remove the list_of_curves column
                formatted_metadata_SC_Calc_loop_curve<-formatted_metadata_SC_Calc_loop_curve[,-which(colnames(formatted_metadata_SC_Calc_loop_curve ) =="list_of_curves")]

                #using previously established single column unique ID for the full data set
                # Merge the LOD-LOQ results on to this dataframe
                formatted_metadata_SC_Calc_loop_curve_temp <- merge(formatted_metadata_built_unique_ID, formatted_metadata_SC_Calc_loop_curve, by = "built_unique_ID", all.x = TRUE)

                #Replace the existing columns in the dataframe with these columns
                formatted_metadata$value[names(formatted_metadata_SC_Calc_loop_curve_temp[-1])] <- formatted_metadata_SC_Calc_loop_curve_temp[-1]

              }#End of loop through standard curve data
            }# end of if std curve data

            # Increment the progress bar, and update the detail text.
            incProgress(1/8, detail = paste("Merging data"))


            #This can't be done without having standard curve results.

            # 10. Calculate the copy number based on the Klymus results for LOD and LOQ
            print("Calculating copy numbers")
            #        copy_numbers[[target]] <- calculate_copy_number(std_w_threshold, raw_data_with_Cq[[target]])
            #        copy_numbers <- calculate_copy_number(std_w_threshold, raw_data_with_Cq[[1]])

            # Increment the progress bar, and update the detail text.
            incProgress(1/8, detail = paste("Merging data"))

            if(upload_data_check$value == "Combine"){
              uploaded_data_working <- as.data.frame(uploaded_data$value)
              #First remove unique identifiers in the old (uploaded_data$value) that are in the raw_multiplex_data
              # which are the data in the RDML file wanting to be combined using the built_unique_ID variable
              uploaded_data_working <- uploaded_data_working[ !uploaded_data_working$built_unique_ID %in% raw_multiplex_data$value$built_unique_ID, ]
              #Then append the records from the new (formatted_metadata) for the new records from
              #formatted_metadata using the built_unique_ID variable on to the old (uploaded_data$value)
              uploaded_data$value<-rbind(as.data.frame(uploaded_data_working), formatted_metadata$value[formatted_metadata$value$built_unique_ID %in% raw_multiplex_data$value$built_unique_ID,])
              updateTabItems(session, "tab_being_displayed", "dashboard")
            }else{
              uploaded_data$value<-formatted_metadata$value
              updateTabItems(session, "tab_being_displayed", "dashboard")
            }#End of the else if using the user response to the overwrite/combine
          }#end of the if/else looking at errors in the data set
          }else if ( complete_calc$value == "ERROR"){
            showModal(modalDialog(
              title = "ERROR - Missing Data",
              "Uploaded data does not contain absorbance data. Please submit a file with complete data."
            ))
        }else if ( complete_calc$value == "NO"){
          uploaded_data$value<-formatted_metadata$value
          updateTabItems(session, "tab_being_displayed", "dashboard")
        }#End of the Complete Calculations if
      })#end of the with Progress block
      #Resetting dataImport file variables
      output$qpcr_file_out <- renderText({as.character("No data selected")})
      output$metadata_file_out <- renderText({as.character("No data selected")})
      qpcr_file$data <- NA
      metadata_file$data <- NA
      upload_data_check$value <- "Initial"
      formatted_metadata$value <- "Initial"
      fdata$value <- "Initial"

      print("At the end data Input and calculations")

    })#End of the upload_data_check$value == "Initial" check
  },ignoreInit = TRUE)#End of observe event process_data

################################################################################
#
#             Could insert a sql data base connection here...
#
#
################################################################################

####################### uploaded_data observe event ############################

#  observeEvent(uploaded_data, {

    #This is where I need to add the connection to the mapping

################### Mapping Dash: Initialize and Populate Filters ##############

######################## Setting up functions for mapping ######################

  #Building the initial map and using the
  #Default static leaflet map before filtering parameters are applied.
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%

      addLegend(position = "bottomright",
                pal = palette_map_ct,
                values =  c("0 < Very strong < 10",
                            "10 <= Strong < 20",
                            "20 <= Moderate < 30",
                            "30 <= Weak < 40",
                            "None > 40"),
                title = 'qPCR Cq Signal Intensity',
                opacity = 0.6) %>%

      #View map full screen (note: only works in web browser)
      addFullscreenControl() %>%


      #Default map view --> Change to Guelph
      setView(lng = -80.2262, lat = 43.5327, zoom = 3) %>%

      #Change leaflet map tiles
      addProviderTiles(providers$Esri.WorldStreetMap)

  })

  #Define the colour Palette for leaflet map
  palette_map_ct <- colorFactor( palette = c("#fbd300",
                                             "#ff8600",
                                             "#ea5f94",
                                             "#9d02d7",
                                             "#0000ff"),
                                 levels = c("0 < Very strong < 10",
                                            "10 <= Strong < 20",
                                            "20 <= Moderate < 30",
                                            "30 <= Weak < 40",
                                            "None > 40"))

###### Reactive variable and observe events for filtering and Mapping ##########

print("Here 11B")

#print(paste0("Here is the uploaded_data$value...", uploaded_data$value))

    #Updated Family list
    family_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$assayFamily)))
      }else {
          return(NULL)
      })
    })

print("Here 12")

    observe({
      updatePickerInput(session, "family_input",
                        choices = family_list(),
                        selected = family_list())
    })

print("Here 13")

    #Updated Genus list
    genus_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$assayGenus)))
      }else{
        return(NULL)
      })
    })

print("Here 14")
    observe({
      updatePickerInput(session, "genus_input",
                        choices = genus_list(),
                        selected = genus_list())
    })

print("Here 15")

    #Updated Species list
    species_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$assaySpecies)))
      }else{
          return(NULL)
      })
    })

print("Here 16")
    observe({
      updatePickerInput(session, "species_input",
                        choices = species_list(),
                        selected = species_list())
    })

print("Here 17")

    #Update qPCR machine list
    machine_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$resultPlatform)))
      }else{
          return(NULL)
      })
    })

print("Here 18")
    observe({
      updatePickerInput(session, "machine_input",
                        choices = machine_list(),
                        selected = machine_list() )
    })

print("Here 19")

    #Update target gene list
    targetGene_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$assayGeneTarget)))
      }else{
          return(NULL)
      })
    })

print("Here 20")
    observe({
      updatePickerInput(session, "targetGene_input",
                        choices = targetGene_list(),
                        selected = targetGene_list() )
    })

print("Here 21")

    #Update project list
    project_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$projectName)))
      }else{
          return(NULL)
      })
    })

print("Here 22")
    observe({
      updatePickerInput(session, "projectID_input",
                        choices = project_list(),
                        selected = project_list() )
    })

print("Here 23")

    #Update assay list
    assay_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        assay_out<-as.character(unique(uploaded_data$value$assayName))
        assay_out<-assay_out[!is.na(assay_out)]
        print("In the assay ractive and the contents are...")
        print(assay_out)
        return(assay_out)
      }else{
          return(NULL)
      })
    })

print("Here 24")
    observe({
      updatePickerInput(session, "assay_input",
                        choices = assay_list(),
                        selected = assay_list())
    })

print("Here 25")
    #Update continent list
    continent_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$projectContinent)))
      }else{
          return(NULL)
      })
    })

print("Here 26")
    observe({
      updatePickerInput(session, "continent_input",
                        choices = continent_list(),
                        selected = continent_list() )
    })

print("Here 27")

    #Update country list
    country_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$projectCountry)))
      }else{
          return(NULL)
      })
    })

print("Here 28")
    observe({
      updatePickerInput(session, "country_input",
                        choices = country_list(),
                        selected = country_list() )
    })

print("Here 29")

    #Update state/province list
    stateProvince_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$projectState)))
      }else{
          return(NULL)
      })
    })

print("Here 30")
    observe({
      updatePickerInput(session, "stateProvince_input",
                        choices = stateProvince_list(),
                        selected = stateProvince_list() )
    })

print("Here 31")

    #Update locality list
    locality_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$projectLocality)))
      }else{
          return(NULL)
      })
    })

print("Here 32")
    observe({
      updatePickerInput(session, "locality_input",
                        choices = locality_list(),
                        selected = locality_list())
    })

print("Here 33")

    #Update establishment means list
    establishmentMeans_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        return(as.character(unique(uploaded_data$value$assayEstabMeans)))
      }else{
          return(NULL)
      })
    })

print("Here 34")
    observe({
      updatePickerInput(session, "establishmentMeans_input",
                        choices = establishmentMeans_list(),
                        selected = establishmentMeans_list() )
    })

print("Here 35")

    #Update Cq intensity list
    cqIntensity_list <- reactive({
      suppressWarnings(if(uploaded_data$value!="Initial"){
        #What Cq value to use based on user select threshold value on Mapping Dashboard.
        if (input$thresholdValueButton == "user"){
          return(unique(uploaded_data$value[,"resultUserProvCq"]))
        }else if (input$thresholdValueButton == "mdmapr"){
          return(unique(uploaded_data$value[,"mdmaprCq"]))
        }
      }else{
        return(NULL)
      })
    })

    observe({
      updatePickerInput(session, "CqIntensity_input",
                        choices = cqIntensity_list(),
                        selected = cqIntensity_list() )
    })

print("Here 37")

#Update minimum date on date range slider
min_date <- reactive({
  suppressWarnings(if(uploaded_data$value!="Initial"){
    return(min(as.Date(uploaded_data$value$replicateCollectionDate, "%Y-%m-%d")[!is.na(as.Date(uploaded_data$value$replicateCollectionDate, "%Y-%m-%d"))]))
  }else{
    return(as.Date("2010-01-01","%Y-%m-%d"))
  })
})

#Update minimum date on date range slider
max_date <- reactive({
  suppressWarnings(if(uploaded_data$value!="Initial"){
    return(max(as.Date(uploaded_data$value$replicateCollectionDate, "%Y-%m-%d")[!is.na(as.Date(uploaded_data$value$replicateCollectionDate, "%Y-%m-%d"))]))
  }else{
    return(as.Date(Sys.Date(), "%Y-%m-%d"))
  })
})

observe({

  updateSliderInput(session, "date_input",
                    min = as.Date(min_date(),"%Y-%m-%d"),
                    max = as.Date(max_date(), "%Y-%m-%d"),
                    value = range(c(as.Date(min_date(),"%Y-%m-%d"),
                                    as.Date(max_date(), "%Y-%m-%d"))),
                    step=1)
})

print("Here 38A")

#Data that will appear in each popup window for the mapped markers.
  popup_data <- reactive({
    suppressWarnings(if(filtered() != "Initial" & nrow(as.data.frame(filtered()))>0){
      data <- as.data.frame(filtered())

      content.output <- paste("<strong><h5>Species:", data$assaySpecies, "</strong>",
                            "<br><h6>Assay:", data$assayName,
                            "<strong><h5> NCBI Taxon ID:", "<a href='https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
                            data$assayTaxonID, "'>", data$assayTaxonID, "</a>", "</strong>",
                            "<strong><h5>Threshold Value:", as.numeric(data$mdmaprThres), "</strong>",
                            "<strong><h5>Cq Value:", ifelse(data[ , cq_column$value]>=40, 0, data[ , cq_column$value]), "</strong>",
#                            "<strong><h5>Copy Number Value:", ifelse(data$copyNumber<=0, 0, exp(data$copyNumber)), "</strong>",
                            "<strong><h5>Copy Number Value:", ifelse(data$resultTemplateConcInCopy<=0, 0, exp(data$resultTemplateConcInCopy)), "</strong>",
                            "<br><h6>Sample:", data$replicateName,
                            "<br><h6>Extract:", data$extractName,
                            "<br><h6>Run:", data$resultRunID,
                            "<br><h6>Well:", data$resultWellLoc,
                            "<br><h6>Platform:", data$resultPlatform,
                            "<br><h6>Collection:", data$replicateCollectionDate,
                            "<h6>Coord(Lat, Lon):", data$stationDecimalLongitude,",", data$stationDecimalLatitude,
                            "<h6>Collector/Analyst:", data$replicateCollectorName,"/",data$extractAnalystName)

print("Here is the string with the HTML to use int eh pop up boxes...")
      return(content.output)
    })
  })


#### Reset button to reset mapping visualizations and filter selections ########

    #Reset uploaded file input and filter widget selections to return map to its default view.
    observeEvent(input$reset, {
      shinyjs::reset("platform")
      shinyjs::reset("qpcr_file")
      shinyjs::reset("metadata_file")
      shinyjs::reset("SCI_fluorescence_file")
      shinyjs::reset("range") #ct range slider
      shinyjs::reset("family_input") #family dropdown box
      shinyjs::reset("genus_input") #genus dropdown box
      shinyjs::reset("species_input") #species dropdown box
      shinyjs::reset("CqIntensity_input") #ct intensity dropdown box
      shinyjs::reset("machine_input") #machine input
      shinyjs::reset("targetGene_input") #target gene (gene symbol) input
      shinyjs::reset("projectID_input") #project ID input
      shinyjs::reset("assay_input") #assay input
      shinyjs::reset("continent_input") #continent input
      shinyjs::reset("country_input") #country input
      shinyjs::reset("stateProvince_input") #state/province input
      shinyjs::reset("locality_input") #locality input
      shinyjs::reset("establishmentMeans_input") #establishment means input

      #Reset date range slider
      updateSliderInput(session, "date_input",
                        value = range(c(min(as.Date("2010-01-01", "%Y-%m-%d")), as.Date(Sys.Date(), "%Y-%m-%d"))))


      #Slider for Cq intensity
      updateSliderInput(session, "range", value(0,40))

      #Clear markers from leaflet map
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups()

    })

############### Building filtered data set for mapping #########################
    #Mapped markers filtered by widgets on dashboard page.
    filtered <- reactive({
print("################################ filtered ################################")

      suppressWarnings(if (uploaded_data$value != "Initial") {

        print("Here in the filtered reactive and the uploaded_data is saved as Global")
        uploaded_data_Global<<-as.data.frame(uploaded_data$value)

        data_final <- as.data.frame(uploaded_data$value)

        #What Cq value to use based on user select threshold value on Mapping Dashboard.
        if (input$thresholdValueButton == "user"){
          print(paste0("Cq column value result...",which( colnames(uploaded_data$value)=="resultUserProvCq" )))
          cq_column$value<-which( colnames(uploaded_data$value)=="resultUserProvCq" )
        }else if (input$thresholdValueButton == "mdmapr"){
          print(paste0("Cq column value result...",which( colnames(uploaded_data$value)=="mdmaprCq" )))
          cq_column$value<-which( colnames(uploaded_data$value)=="mdmaprCq" )
        }

        #First remove all records with no coordinates and no Cq
        data_final<-data_final[!is.na(data_final$stationDecimalLongitude),]
        data_final<-data_final[!is.na(data_final$stationDecimalLatitude),]
        data_final<-data_final[!is.na(data_final[ , cq_column$value]),]
print(paste0("Here are the number of records in the data_final - 3 - ", nrow(data_final)))
data_final_Global1<<-data_final
        #Keep records within the selected Cq Range
        data_final <- data_final[data_final[,cq_column$value] >= input$range[1],]
        data_final <- data_final[data_final[,cq_column$value] <= input$range[2],]
print(paste0("Here are the number of records in the data_final - 3 - ", nrow(data_final)))
data_final_Global2<<-data_final
        #Keep records within the selected date Range
        data_final <- data_final[data_final$replicateCollectionDate >= input$date_input[1],]
        data_final <- data_final[data_final$replicateCollectionDate <= input$date_input[2],]

print(paste0("Here are the number of records in the data_final - 3 - ", nrow(data_final)))
data_final_GlobalA<<-data_final

        #Looking at all of the other filtering criteria
        data_final<-data_final[data_final$assayName %in% input$assay_input,]
print(paste0("Here are the number of records in the data_final - 3 - ", nrow(data_final)))
data_final_GlobalB<<-data_final
        data_final<-data_final[data_final$assayFamily %in% input$family_input,]
print(paste0("Here are the number of records in the data_final - 4 - ", nrow(data_final)))
data_final_GlobalC<<-data_final
        data_final<-data_final[data_final$assayGenus %in% input$genus_input,]
print(paste0("Here are the number of records in the data_final - 5 - ", nrow(data_final)))
data_final_GlobalD<<-data_final
        data_final<-data_final[data_final$assaySpecies %in% input$species_input,]
print(paste0("Here are the number of records in the data_final - 6 - ", nrow(data_final)))
data_final_GlobalE<<-data_final
        data_final<-data_final[data_final$resultPlatform %in% input$machine_input,]
print(paste0("Here are the number of records in the data_final - 7 - ", nrow(data_final)))
data_final_GlobalF<<-data_final
        data_final<-data_final[data_final$assayGeneTarget %in% input$targetGene_input,]
print(paste0("Here are the number of records in the data_final - 8 - ", nrow(data_final)))
data_final_GlobalG<<-data_final
        data_final<-data_final[data_final$projectName %in% input$projectID_input,]
print(paste0("Here are the number of records in the data_final - 9 - ", nrow(data_final)))
data_final_GlobalH<<-data_final
        data_final<-data_final[data_final$projectCountry %in% input$country_input,]
print(paste0("Here are the number of records in the data_final - 10 - ", nrow(data_final)))
data_final_GlobalI<<-data_final
        data_final<-data_final[data_final$projectContinent %in% input$continent_input,]
print(paste0("Here are the number of records in the data_final - 11 - ", nrow(data_final)))
data_final_GlobalJ<<-data_final
        data_final<-data_final[data_final$projectState %in% input$stateProvince_input,]
print(paste0("Here are the number of records in the data_final - 11 - ", nrow(data_final)))
#        data_final<-data_final[data_final$assayEstabMeans %in% input$establishmentMeans_input,]
print(paste0("Here are the number of records in the data_final - 13 - ", nrow(data_final)))
data_final_GlobalK<<-data_final
        data_final<-data_final[data_final$mdmaprCq %in% input$CqIntensity_input,]
print(paste0("Here are the number of records in the data_final - 14 - ", nrow(data_final)))
data_final_GlobalL<<-data_final

        if(nrow(data_final)>0){
          return(data_final)
        }else{
          return("Initial")
        }
      }else{
        return("Initial")
      })#End of if with suppress warnings
    })


######## Mapping and Filtering Data tab observe events #########################

print("Here 39")

    observeEvent(input$tab_being_displayed,{

      if(input$tab_being_displayed == "dashboard") {

        #Set up the value of the cq slider based on the data in the uploaded data set
        suppressWarnings(if(uploaded_data$value!="Initial"){
          #Get the total number of cycles used for the standard curve first select only cycle columns
          cq_max <- uploaded_data$value[,grep("Cycle", names(uploaded_data$value), value = TRUE)]
          #Remove columns with only NA values
          cq_max <- ncol(cq_max[,colSums(is.na(cq_max))<nrow(cq_max)])
          updateSliderInput(session, "range",
                            min = 0,
                            max = cq_max,
                            value = c(0,cq_max),
                            step=0.10)
        })

        updateTabsetPanel(session, "map_filter_table_tabbox", "Mapping")

        suppressWarnings(if(filtered() != "Initial" & nrow(as.data.frame(filtered()))>0){
print("In the observe event map_filter_table_tabbox 5 ...")
print(paste0("Here is the value for the column for colour mapping..."))
print(filtered()[,cq_column$value])

          leaflet::leafletProxy("mymap", data = as.data.frame(filtered())) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            clearPopups() %>%
            #Adding labels to markers on map
            addCircleMarkers(lng = ~stationDecimalLongitude,
                             lat = ~stationDecimalLatitude,
                             color = ~palette_map_ct(as.data.frame(filtered())[,cq_column$value]),
                             clusterOptions = markerClusterOptions(),
                             popup= popup_data())

        }else{
          #If there are no values then ensure that the map is cleared of plotted points
          leaflet::leafletProxy("mymap", data = as.data.frame(filtered())) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            clearPopups()
        })
      }
    })

    observeEvent(input$map_filter_table_tabbox,{

      if(input$map_filter_table_tabbox == "Mapping") {
        suppressWarnings(if(filtered() != "Initial" & nrow(as.data.frame(filtered()))>0){
          leaflet::leafletProxy("mymap", data = as.data.frame(filtered())) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            clearPopups() %>%
            #Adding labels to markers on map
            addCircleMarkers(lng = ~stationDecimalLongitude,
                             lat = ~stationDecimalLatitude,
                             color = ~palette_map_ct(as.data.frame(filtered())[,cq_column$value]),
                             clusterOptions = markerClusterOptions(),
                             popup= popup_data())

        }else{
          #If there are no values then ensure that the map is cleared of plotted points
          leaflet::leafletProxy("mymap", data = as.data.frame(filtered())) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            clearPopups()
        })
      }else if(input$map_filter_table_tabbox == "Data Filtering"){

        suppressWarnings(if(uploaded_data$value!="Initial"){
          #What Cq value to use based on user select threshold value on Mapping Dashboard.
          if (input$thresholdValueButton == "user"){
            return(unique(uploaded_data$value[,"resultUserProvCq"]))
          }else if (input$thresholdValueButton == "mdmapr"){
            return(unique(uploaded_data$value[,"mdmaprCq"]))
          }
        }else{
          return(NULL)
        })



      }else if(input$map_filter_table_tabbox == "Mapped Data Table"){
        suppressWarnings(if(filtered() != "Initial" & nrow(as.data.frame(filtered()))>0){
          #Dynamic data table with Mapping marker information for the mapping table
          # Mapped Data Table tab on the mapping dashboard
          output$mapping_data <- renderDataTable({
            suppressWarnings(if (uploaded_data$value != "Initial") {
              datatable(as.data.frame(filtered())[,-1],
                        options = list(pageLength = 5,
                          scrollX = TRUE
                        ))
            })#End of if and suppress warnings
          })
        })
      }
print("In the observe event map_filter_table_tabbox 6 ...")
    })



    #Download button for downloading CSV of filtered mapping data
    #    output$downloadFilteredData <- downloadHandler(

    #      filename = 'MDMAP_Mapping_data.csv',

    #      content = function(file) {

    #        write.csv(filtered(), file,  row.names=FALSE)
    #    })



















################### Standard Curve Design page #################################


################### Standard Curve File Validation #############################

    #Return popup message regarding uploaded standard curve fluorescence file.
#    observeEvent(input$SC_fluorescence_file,
#                 std_fluorescence_file_validation_msgs(input$SC_fluorescence_file))

    #Return popup messaged regarding uploaded metadata file.
#    observeEvent(input$SC_metadata_file,
#                 std_metadata_file_validation_msgs(input$SC_metadata_file))


    standard_curve_tab_data <- reactiveVal(value = NULL)

#    observeEvent(input$Uploaded_SC_submit, {
#      isolate(

        #Validate content of user uploaded files
#        if (user_uploaded_standard_curve_file_validation(input$SC_fluorescence_file,
#                                                         input$SC_metadata_file,
#                                                         input$SC_platform) == TRUE)

#        {return(NULL)}


        ############ Standard Curve Data Processing ##########
#        else{

#          if (input$SC_platform == "Biomeme two3/Franklin") {

            #Read in raw qPCR data
#            qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$SC_fluorescence_file$datapath))

            #Read in metadata
#            metadata_biomem23 <-  format_standardCurve_metadata(read_xlsx(input$SC_metadata_file$datapath, sheet = 5))

            #Function to process and merge files
#            merged_biomem23_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_biomem23_raw, metadata_biomem23)

            #Function to calculate LOD and LOQ
#            merged_biomem23_file <- calculate_SC_LOD_LOQ(merged_biomem23_file, input$LOQthres)


            #This merged datatable that will be used to populate map
#            return(standard_curve_tab_data(merged_biomem23_file))

#          }


          ### Machine Option 2: MIC ###
#          else if (input$SC_platform == "MIC/BioRad") {

            #Read in raw qPCR data
#            qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$SC_fluorescence_file$datapath))
#            print("raw data processes")
            #print(qpcr_MIC_raw)

            #Read in metadata
#            metadata_MIC <- format_standardCurve_metadata(read_xlsx(input$SC_metadata_file$datapath, sheet = 5))
#            print("metadata processed")
            #print(metadata_MIC)

            #Function to process and merge files
#            merged_mic_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_MIC_raw,metadata_MIC)
            #print(merged_mic_file)
#            print("file merged")

            #Function to calculate LOD and LOQ
#            merged_mic_file <- calculate_SC_LOD_LOQ(merged_mic_file, input$LOQthres)
#            print("LOD Calculated")
            #print(merged_mic_file)

            #This merged datatable that will be used to populate map
#            return(standard_curve_tab_data(merged_mic_file))

#          }

          ### Machine Option 3: StepOnePlus ###
#          else if (input$SC_platform == "StepOnePlus") {

            #Read in standard curve fluorescence file
#            standardCurve_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$SC_fluorescence_file$datapath, sheet = 4))

            #Read in standard curve metadata
#            standardCurve_metadata_StepOnePlus <- format_standardCurve_metadata(read_xlsx(input$SC_metadata_file$datapath, sheet = 5))

#            #Function to process and merge files
#            merged_StepOnePlus_file <- merge_standardCurve_metadata_fluorescence_file(standardCurve_StepOnePlus_raw, standardCurve_metadata_StepOnePlus)

#            #Function to calculate LOD and LOQ
#            merged_StepOnePlus_file <- calculate_SC_LOD_LOQ(merged_StepOnePlus_file, input$LOQthres)
#            print(merged_StepOnePlus_file)

#            return(standard_curve_tab_data(merged_StepOnePlus_file))
#          }
#        })
#    })

    # show the LOD warning Message
    output$text <- renderPrint({
      req(standard_curve_tab_data())
      noquote(standard_curve_tab_data()$LODWarning[1])})

    SC_well_list <- reactive({

      if (!is.null(standard_curve_tab_data())) {

        data <- as.data.frame(standard_curve_tab_data())

        well_data <- append('None', as.character(unique(data$wellLocation)))

        return(well_data)}

      else {return(NULL)}
    })
    #
    observe({updateSelectInput(session,
                               "SC_wells",
                               choices = SC_well_list(),
                               selected = 'None')})

    SC_assay_list <- reactive({

      if (!is.null(uploaded_data$value)) {

        data <- as.data.frame(uploaded_data$value)

        assay_data <- append('None', as.character(unique(data$assayName)))

        return(assay_data)}

      else {return(NULL)}
    })
    #
    observe({updatePickerInput(session,
                               "DA_assay_input",
                               choices = SC_assay_list(),
                               selected = 'None')})


    ################### Standard Curve Table and Plot Creation #####################

    #Standard curve plot only appears when submit button is pressed
    observeEvent(input$Uploaded_SC_submit | input$submit_zip, isolate({

      output$standardCurve_plot <- renderPlotly({
        req(standard_curve_tab_data())

        #Add data to plot
        SC_plot_data <- standard_curve_tab_data()

        #Remove control records
        SC_plot_data <- control_records_to_remove(SC_plot_data)

        #Change standard concentration value to numeric and then take log value
        SC_plot_data$standardConc <- as.numeric(SC_plot_data$standardConc)
        SC_plot_data$standardConc <- log(SC_plot_data$standardConc)

        #Change user provided Cq to numeric value to numeric
        SC_plot_data$systemCalculatedCqValue <- as.numeric(SC_plot_data$systemCalculatedCqValue)
        SC_plot_data$systemCalculatedLOQ<- as.numeric(SC_plot_data$systemCalculatedLOQ)
        SC_plot_data$systemCalculatedLOD<- as.numeric(SC_plot_data$systemCalculatedLOD)

        #Add column with residual values to data set
        regression_line <- lm(as.numeric(systemCalculatedCqValue) ~ as.numeric(standardConc), SC_plot_data)
        SC_plot_data$Residual <- abs(residuals(regression_line))

        #Code to get R squared
        #Adapted from: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
        # Code to get equation of the line and R-squared
        # Adapted from: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

        lm_eq <- function(df){

          model1 <- lm(systemCalculatedCqValue ~ standardConc, df, na.action=na.exclude)

          b = format(unname(coef(model1)[1]), digits = 2)

          mx = paste("(", format(unname(coef(model1)[2]), digits = 2), "x", ")",  sep = "")

          r2 = format(summary(model1)$r.squared, digits = 3)

          equation_of_line <- paste("y", " = ", mx, " + ", b, ",   ", "R-squared", " = ", r2,  sep = "")

          return (equation_of_line)

        }



        print(

          ggplotly(height = 700,

                   ggplot(data = SC_plot_data, aes(x = standardConc,
                                                   y = systemCalculatedCqValue,
                                                   color = Residual)) +

                     geom_point(size = 10, alpha = .3) +

                     geom_smooth(method = "lm",
                                 se = FALSE,
                                 alpha = .15,
                                 color = 'black',
                                 formula = y ~ x) +

                     geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOD),
                                color = "#ea5f94",
                                linetype="dashed") +
                     #
                     geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOQ),
                                color = "#0178A1",
                                linetype="dotted") +

                     geom_text(aes(x= log(systemCalculatedLOD),
                                   label="LOD",
                                   y= mean(systemCalculatedCqValue)),
                               colour="#ea5f94",
                               angle=90,
                               vjust = 1.2,
                               size=5) +

                     geom_text(aes(x= log(systemCalculatedLOQ),
                                   label="LOQ",
                                   y= mean(systemCalculatedCqValue)*1.2),
                               colour="#0178A1",
                               angle=90,
                               vjust = 1.2,
                               size=5) +

                     geom_text(aes(label=wellLocation)) +


                     xlab("log DNA Copy Number") +

                     ylab("System Calculated Cq") +

                     theme_minimal() +

                     scale_color_gradient(low = "#fbd300", high = "#0000ff") +

                     theme( axis.title.x = element_text( size=13),
                            axis.title.y = element_text( size=13),
                            axis.text.x = element_text(size=12),
                            axis.text.y = element_text(size=12),
                            plot.title = element_text(hjust = 0.5, size=18)) +

                     # ggtitle(paste0("Standard Curve for ", input$SC_project_input,  "\n",
                     #                lm_eq(SC_plot_data)))
                     ggtitle(paste0("Standard Curve ",
                                    lm_eq(SC_plot_data)))

          ) %>%

            layout(margin = list(l=50, r=50, b=100, t=100, pad=4),
                   annotations = list(x = 1.1, y = -0.17,
                                      text = "Source: MDMAPR-CC-BY",
                                      showarrow = F,
                                      xref='paper',
                                      yref='paper',
                                      font=list(size=12, color="darkblue"))))
      })
    }))

    observeEvent(input$Uploaded_SC_reset, {
      shinyjs::reset("SC_platform")
      updateSelectInput(session,
                        inputId = "SC_platform",
                        label = "qPCR Platform",
                        choices = c("None",
                                    "StepOnePlus",
                                    "Biomeme two3/Franklin",
                                    "MIC/BioRad"),
                        selected = "None")

      shinyjs::reset("SC_fluorescence_file")
      shinyjs::reset("SC_metadata_file")
      shinyjs::reset("SC_overview_table")
    })

    ################### Standard Curve Re-calibration based on wells selected ######
    observeEvent(input$std_recalib, isolate({

      output$standardCurve_plot <- renderPlotly({
        req(standard_curve_tab_data())
        #filter data based on well selected
        SC_plot_data <- standard_curve_tab_data()
        SC_plot_data <- SC_plot_data[SC_plot_data$wellLocation==input$SC_wells, ]

        #Remove control records
        SC_plot_data <- control_records_to_remove(SC_plot_data)

        #Change standard concentration value to numeric and then take log value
        SC_plot_data$standardConc <- as.numeric(SC_plot_data$standardConc)
        SC_plot_data$standardConc <- log(SC_plot_data$standardConc)

        #Change user provided Cq to numeric value to numeric
        SC_plot_data$systemCalculatedCqValue <- as.numeric(SC_plot_data$systemCalculatedCqValue)
        SC_plot_data$systemCalculatedLOQ<- as.numeric(SC_plot_data$systemCalculatedLOQ)
        SC_plot_data$systemCalculatedLOD<- as.numeric(SC_plot_data$systemCalculatedLOD)

        #Add column with residual values to data set
        regression_line <- lm(as.numeric(systemCalculatedCqValue) ~ as.numeric(standardConc), SC_plot_data)
        SC_plot_data$Residual <- abs(residuals(regression_line))

        #Code to get R squared
        #Adapted from: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
        # Code to get equation of the line and R-squared
        # Adapted from: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

        lm_eq <- function(df){

          model1 <- lm(systemCalculatedCqValue ~ standardConc, df, na.action=na.exclude)

          b = format(unname(coef(model1)[1]), digits = 2)

          mx = paste("(", format(unname(coef(model1)[2]), digits = 2), "x", ")",  sep = "")

          r2 = format(summary(model1)$r.squared, digits = 3)

          equation_of_line <- paste("y", " = ", mx, " + ", b, ",   ", "R-squared", " = ", r2,  sep = "")

          return (equation_of_line)

        }



        print(

          ggplotly(height = 700,

                   ggplot(data = SC_plot_data, aes(x = standardConc,
                                                   y = systemCalculatedCqValue,
                                                   color = Residual)) +

                     geom_point(size = 10, alpha = .3) +

                     geom_smooth(method = "lm",
                                 se = FALSE,
                                 alpha = .15,
                                 color = 'black',
                                 formula = y ~ x) +

                     geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOD),
                                color = "#ea5f94",
                                linetype="dashed") +
                     #
                     geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOQ),
                                color = "#0178A1",
                                linetype="dotted") +

                     geom_text(aes(x= log(LOD),
                                   label="LOD",
                                   y= mean(systemCalculatedCqValue)),
                               colour="#ea5f94",
                               angle=90,
                               vjust = 1.2,
                               size=5) +

                     geom_text(aes(x= log(LOQ),
                                   label="LOQ",
                                   y= mean(systemCalculatedCqValue)*1.2),
                               colour="#0178A1",
                               angle=90,
                               vjust = 1.2,
                               size=5) +

                     geom_text(aes(label=wellLocation)) +


                     xlab("log DNA Copy Number") +

                     ylab("System Calculated Cq") +

                     theme_minimal() +

                     scale_color_gradient(low = "#fbd300", high = "#0000ff") +

                     theme( axis.title.x = element_text( size=13),
                            axis.title.y = element_text( size=13),
                            axis.text.x = element_text(size=12),
                            axis.text.y = element_text(size=12),
                            plot.title = element_text(hjust = 0.5, size=18)) +

                     # ggtitle(paste0("Standard Curve for ", input$SC_project_input,  "\n",
                     #                lm_eq(SC_plot_data)))
                     ggtitle(paste0("Standard Curve ",
                                    lm_eq(SC_plot_data)))

          ) %>%

            layout(margin = list(l=50, r=50, b=100, t=100, pad=4),
                   annotations = list(x = 1.1, y = -0.17,
                                      text = "Source: MDMAPR-CC-BY",
                                      showarrow = F,
                                      xref='paper',
                                      yref='paper',
                                      font=list(size=12, color="darkblue"))))
      })
    }))

    ################### Standard Curve Table Output ################################
    output$SC_overview_table  <- renderDataTable({

      data <- standard_curve_tab_data()[ , -c(1, 2, 3, 4, 102)]

      datatable(data,
                options = list(scrollX = TRUE,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = '500px', targets = c(84)))))})




    ################### Low Quant eDNA LOD Method ##################################
    output$lowquant_standardCurve_plot <- renderPlot({
      req(standard_curve_tab_data())

      DAT <- standard_curve_tab_data()[, c("standardCurveName", "runRecordedBy", "systemCalculatedCqValue", "standardConc")]
      # rename the columns to match the calculation
      colnames(DAT) <- c("Target", "Lab", "Cq", "SQ")

      ## Ensure data is in the proper format:
      DAT$Target <- as.factor(DAT$Target)
      DAT$Lab <- as.factor(DAT$Lab)  #ML
      DAT$Cq <- suppressWarnings(as.numeric(as.character(DAT$Cq))) #Non-numerical values (i.e. negative wells) will be converted to NAs
      DAT$SQ <- suppressWarnings(as.numeric(as.character(DAT$SQ))) #Non-numerical values (i.e. NTC) will be converted to NAs
      # by MDMAPR Definitions, if a Cq value is 40, there is no amplification and should be converted to NA
      DAT$Cq[which(DAT$Cq==40)] <- NA

      # setting all negative controls to 0
      DAT$SQ[is.na(DAT$SQ)] <- 0
      DAT.df <- data.frame(DAT)


      # compute poisson estimates
      DAT.Tar.SQ <-  DAT.df %>%
        group_by(Target, SQ) %>%
        dplyr::summarise(detect=sum(!is.na(Cq)), n=n(),  Cqmean=mean(Cq, na.rm=TRUE),
                         Lab=Lab[1])
      DAT.Tar.SQ <- droplevels(data.frame(DAT.Tar.SQ))
      uLabs <- unique(DAT.Tar.SQ$Lab) #unique labs

      DAT.Tar.SQ <- arrange(DAT.Tar.SQ, Lab, Target, SQ) #sort data by SQ in Target in Lab

      ## Add variables to data set:  L10.SQ, phat, ...
      DAT.Tar.SQ <- within(DAT.Tar.SQ, {
        L10.SQ <- log10(SQ)
        phat <- detect/n           #sample proportion detect
        vphat <- phat*(1-phat)/n   #var of phat
        lamhat <- -log(1-phat)
        vlamhat <- phat/n/(1-phat)  #var of lamhat using the delta method
        sdlamhat <- sqrt(vlamhat)   #sd of lamhat using the delta method
        MElamhat <- 1.96*sdlamhat  #margin of error for lambda hat using delta method
      }
      )
      ## All Targets and Labs **DO NOT DUPLICATE Target names over Labs!!
      uLabs <- unique(DAT.Tar.SQ$Lab)
      uTargets <- unique(DAT.Tar.SQ$Target)
      nTargets <- length(uTargets)
      uLabsTargets <- unique(DAT.Tar.SQ[,c('Lab','Target')])
      uLabsTargets$Lab <- as.character(uLabsTargets$Lab)

      #ensure ulabsTargets in same order as uTargets
      uLabsTargets <- uLabsTargets[match(uLabsTargets$Target, uTargets),]
      uLabsTargets.names <- apply(uLabsTargets, 1, paste, collapse=', ')

      DAT.Tar.SQ <- within(DAT.Tar.SQ, {
        CIexphat.lower <-  1 - qbeta(.975, n-detect+1, detect)  #exact phat bounds
        CIexphat.upper <-  qbeta(.975, detect+1, n-detect)

        ## Use transformed exact phat bounds
        Lamhatex.Lower <- -log(1 - CIexphat.lower)
        Lamhatex.Upper <- -log(1 - CIexphat.upper)
      }
      )
      print("got here2")
      nndetect <- vector("list", nTargets)
      nrowTarget <- rep(0, length=nTargets)

      for(i in 1:nTargets) {

        print(paste0(nTargets, "is there a mistake"))

        Target.dat <- subset(DAT.Tar.SQ, Target==uTargets[i])
        print(Target.dat)
        bSQ <- !is.na(Target.dat$phat)
        lastSQ <- as.logical(cumprod(Target.dat$phat!=1 & bSQ))
        ## removes first observations with SQ with phat=1 and larger SQs
        Target.dat <- Target.dat[lastSQ,]
        print(Target.dat)
        print("with last sq")
        nndetect[i] <- list(Target.dat )
        nrowTarget[i] <- nrow(Target.dat)

        print(nrow(nndetect[[i]]))

        if(nrow(nndetect[[i]]) < 2) {print("we got here");next}

        maxSQ <- max(Target.dat$SQ)
        maxlamhat <- max(Target.dat$lamhat)

        ######################### show this plot on the standard curve tab
        print("got here 3")
        plot(Target.dat$SQ, Target.dat$lamhat, xlog=TRUE, ylab='Lambda hat',
             xlab='Starting copy number',
             ylim=c(0, maxlamhat), xlim=c(0, maxSQ), main=uLabsTargets.names[i])
        ## Transformed Exact CI
        arrows(Target.dat$SQ, Target.dat$Lamhatex.Lower, Target.dat$SQ,
               Target.dat$Lamhatex.Upper,
               length=0.05, angle=90, code=3)
        ## overlay simple regression line and R-squared
        jlm <- lm(lamhat ~ SQ, data=Target.dat)
        abline(jlm, col=2)
        legend("topleft", paste('lm Rsq=',round(summary(jlm)$r.squared, 2)), bty="n")
        print("got here 4")

      }
    })



    ################### qPCR Data Overview page #####################################

    ##Presence Absence table

    # Filter options on data overview page
    #Update assay list on page when additional file is uploaded
    DA_assay_list <- reactive({

      if (!is.null(uploaded_data$value)) {

        data <- as.data.frame(uploaded_data$value)

        assay_data <- append('None', as.character(unique(data$assayName)))

        return(assay_data)}

      else {
        return(NULL)}
    })

    observe({updatePickerInput(session,
                               "DA_assay_input",
                               choices = DA_assay_list(),
                               selected = 'nuBrook Trout TripleLock')})

    #Update project list based on assay selection
    DA_project_list <- reactive({

      if (input$DA_machine_input != 'None') {

        data <- as.data.frame(uploaded_data$value)

        updated_list <- data[data$assayName == input$DA_assay_input, ]
        updated_list <- updated_list[updated_list$runPlatform == input$DA_machine_input, ]
        project_list <-  as.character(unique(updated_list$projectName))

        return(project_list)}

      else {return(NULL)}
    })


    observe({updatePickerInput(session,
                               "DA_project_input",
                               choices = append('None', DA_project_list()),
                               selected = 'None') })


    #Transform dataframe for presence/absence table based on filtered
    presence_absence_table_data <- reactive({

      if (!is.null(uploaded_data$value)) {

        data <- uploaded_data$value
        PA_filtered_data <- data[data$assayName == input$DA_assay_input, ]
        PA_filtered_data <- PA_filtered_data[PA_filtered_data$runPlatform == input$DA_machine_input, ]
        PA_filtered_data <- PA_filtered_data[PA_filtered_data$projectName == input$DA_project_input, ]

      }
    })

    what_clr <- function(value) {
      if (value >= input$cqValueCutoff)
      {return ("#8FBACB")}

      else
      {return("#ffb14e")}
    }


    available_threshold <- function(value) {
      if (value == "Unable to Determine Threshold")
      {return ("#ffd700")}
    }

    # #Presence/absence table
    observeEvent(input$submit, isolate ({

      output$presence_absence_table <- renderReactable({
        data <- as.data.frame(presence_absence_table_data())

        prescence_abscence_table <- data[ , c("projectName", "runID", "extractName", "control", "geneSymbol", "runPlatform", "wellLocation", "userProvidedThresholdValue", "userProvidedCqValue", "systemCalculatedThresholdValue", "systemCalculatedCqValue" )]

        reactable(prescence_abscence_table,

                  #Table columns
                  columns = list(

                    projectName = colDef(name = "Project Name",align = "center", width = 300),
                    runID = colDef(name = "Plate ID", align = "center"),
                    extractName = colDef(name = "Sample Name", align = "center", width = 200),
                    control = colDef(name = "Control", align = "center"),
                    geneSymbol = colDef(name = "Gene", align = "center"),
                    runPlatform = colDef(name = "Machine", align = "center"),
                    wellLocation = colDef(name = "Well Location", align = "center", width = 200),

                    userProvidedThresholdValue = colDef(name = "User Provided Threshold",
                                                        align = "center",
                                                        width = 300),

                    # userProvidedCqValue = colDef(name = "User Provided Cq Value",
                    #                              width = 250,
                    #                              style = function(value) {
                    #                                color  <- what_clr(value)
                    #                                list(background = color)}),

                    systemCalculatedThresholdValue = colDef(name = "System Calculated Threshold",
                                                            width = 300,
                                                            align = "center",
                                                            style = function(value) {
                                                              color  <- available_threshold(value)
                                                              list(background = color)}),

                    systemCalculatedCqValue = colDef(name = "System Calculated Cq Value",
                                                     width = 250,
                                                     style = function(value) {
                                                       color  <- what_clr(value)
                                                       list(background = color)})),

                  #Filter each column by text
                  filterable = TRUE,

                  #Type in page number to jump to a page
                  paginationType = "jump",

                  #Minimum rows shown on page
                  minRows = 20,

                  #Number of rows to show
                  defaultPageSize = 20,

                  #Adding outline around cells
                  outlined = TRUE,

                  #Color every other row
                  striped = TRUE,

                  #Hover above row to highlight it
                  highlight = TRUE,

                  #Default record selected from table
                  defaultSelected = 1,

                  #Check box
                  selection = "single",

                  #Wrap text in column
                  wrap = FALSE,

                  theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee",
                                                                 boxShadow = "inset 2px 0 0 0 #ffa62d"))
        )
      })
    }))
    #
    #
    # ## Amplification plot
    #
    # #Get selected row for amplification plot
    selected <- reactive(getReactableState("presence_absence_table", "selected"))
    #
    # #Created amplifcation plot based on selected well sample
    #
    observeEvent(input$submit, isolate ({

      output$selected <-  renderPlotly({

        #Created dataframe of filtered presence/absence data
        data <- as.data.frame(presence_absence_table_data())[selected(), ]

        #Create data frame for amplification curve
        amp_curve_data <- na.omit(as.data.frame(t(data[ , c(18:87)])))
        colnames(amp_curve_data) <- "Fluorescence"
        amp_curve_data$cycles <- c(1:nrow(amp_curve_data))


        #Created plot
        print(
          ggplotly(height = 700,

                   ggplot(amp_curve_data, aes(x = cycles, y = as.numeric(Fluorescence))) +

                     geom_point(aes(colour = "Absorbances") , size = 2) +

                     geom_hline(aes(yintercept = as.numeric(data$userProvidedThresholdValue),
                                    color = "User Provided Threshold"),
                                linetype="dashed", size = 1) +

                     geom_hline(aes(yintercept = as.numeric(data$systemCalculatedThresholdValue),
                                    color = "System Calculated Threshold"),
                                linetype="dotted", size = 1) +


                     ggtitle(paste0( "Well ", data$wellLocation, " Amplification Curve")) +

                     labs(x = " Cycle", y = "Absorbance") +

                     theme_gray() +

                     scale_colour_manual("",
                                         breaks = c("Absorbances",
                                                    "User Provided Threshold",
                                                    "System Calculated Threshold"),
                                         values = c("User Provided Threshold"="#ea5f94",
                                                    "Absorbances"="#0000ff",
                                                    "System Calculated Threshold"="#ff8600")) +

                     theme(plot.title = element_text(hjust = 0.5, size=18),
                           axis.title.x = element_text( size=13),
                           axis.title.y = element_text( size=13),
                           axis.text.x = element_text(size=12),
                           axis.text.y = element_text(size=12),
                           legend.text = element_text(size = 10),
                           legend.background = element_rect(fill="lightblue")))  %>%

            layout(legend = list(orientation = "h", x = 0.02, y = -0.16), #legend position
                   margin = list(l=50, r=60, b=140, t=100, pad=4),
                   annotations = list(x = 1, y = -0.31,
                                      text = "Source: MDMAPR-CC-BY",
                                      showarrow = F,
                                      xref='paper',
                                      yref='paper',
                                      font=list(size=12,
                                                color="darkblue"))))})
    }))

    #
    #    ## welcome page (Create downloadable metadata template) -----
    #
    #    #Created  metadata template for users to download
    #    project_sheet <- data.frame(matrix(ncol = 24, nrow = 1))

    #    colnames(project_sheet) <- c("projectID", "projectCreationDate","projectName","projectRecordedBy","projectOwner","projectContactEmail","projectDescription","InstitutionID","projectDataNotes","geographicRegionID", "continent","country", "stateProvince","municipality","siteID", "locality","estimatedPerimeter","estimatedSurfaceArea(m2)","siteType","siteLength(m2)", "stationID","stationName", "decimalLongitude", "decimalLatitude")


    #    replicate_sheet <- data.frame(matrix(ncol = 55, nrow = 1))
    #    colnames(replicate_sheet) <- c("replicateID", "stationID", "collectorName","replicateName","collectionDate","collectionTime","storageID","DateOfStorage","methodOfStorage","minimumElevationInMeters","maximumElevationInMeters","verbatimElevation","minimumDepthInMeters","maximumDepthInMeters","verbatimDepth","flowRate(m/s)", "filterType","filtrationDuration(mins)","volumeFiltered","processLocation","replicationNumber","riparianVegetationPercentageCover","dissolvedOxygen(mg/L)","waterTemperature(C)","pH","TSS(mg/L)","EC(uS/cm)","turbidity(NTU)","discharge","tide","chlorophyl","salinity(ppt)","contaminants(ng/g)","traceMetals(mg/kg)","organicContent(%)","microbialActivity","grainSize","replicateDataNotes", "extractID", "extractName","analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes","tubePlateID","frozen", "fixed","dnaStorageLocation","extractMethodOfStorage","dnaVolume","quantificationMethod", "concentration(ng/ul)")


    #    assay_sheet <- data.frame(matrix(ncol = 30, nrow = 1))
    #    colnames(assay_sheet) <- c( "assayID", "establishmentMeans","assayName","assayOwnership","assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol","dilutions", "replicates", "primerR", "primerF", "probe","ampliconLength (bp)", "probeFluorescentTag", "dye(s)","quencher","probeModification", "taxonID", "kingdom","phylum","class","order","family", "genus", "subgenus", "species", "vernacularName","organismScope")

    #    results_sheet <- data.frame(matrix(ncol = 29, nrow = 1))
    #    colnames(results_sheet) <- c("resultID","assayID", "extractID", "wellLocation","sampleName", "copyNumber", "control", "userProvidedThresholdValue", "userProvidedCqValue", "runID", "runRecordedBy", "runDate", "runTime","runPlatform","machineID", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")

    #    standardCurveResults_sheet <- data.frame(matrix(ncol = 36, nrow = 1))
    #    colnames(standardCurveResults_sheet ) <- c("SCresultID","wellLocation","sampleName", "copyNumber", "control","standardConc",   "userProvidedThresholdValue", "userProvidedCqValue","runID", "runRecordedBy", "runDate", "runTime", "runPlatform","machineID", "standardCurveID","assayID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes", "LOD","LOQ", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")


    #    data_list <- (list( project_Table = project_sheet,
    #                        replicate_Table = replicate_sheet,
    #                        assay_Table = assay_sheet,
    #                        results_Table = results_sheet,
    #                        standardCurveResults_Table = standardCurveResults_sheet))

    #    output$downloadTemplate <- downloadHandler(
    #      filename = 'MDMAPR_metadata_template.xlsx',
    #      content = function(file) {write_xlsx(data_list, file)})




    ################### Data Modelling page ########################################

    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = paste("report", sep = ".", "html"),
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(n = input$selectData)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport,
                          output_file = file,

                          envir = new.env(parent = globalenv()))
      }
    )

    # initialize the reactive value
    InputDataset <- reactiveVal(NULL)

    # read in the uploaded csv file, also removing all columns that are all NA
    observeEvent(input$submit_modelling,{
      modelling_data <- as.data.frame(read.csv(input$model_data$datapath))
      # need to convert all NULL to NA
      modelling_data[modelling_data=="NULL"] <- NA
      modelling_data <- modelling_data[colSums(!is.na(modelling_data)) > 0]

      # convert all character variables into factor
      modelling_data[sapply(modelling_data, is.character)] <- lapply(modelling_data[sapply(modelling_data, is.character)],
                                                                     as.factor)
      # remove the columns that have only one value throughout.
      modelling_data <- modelling_data[vapply(modelling_data, function(x) length(unique(x)) > 1, logical(1L))]
      return(InputDataset(modelling_data))

    })


    InputDataset_model <- reactive({
      if (is.null(input$SelectX)) {
        dt <- as.data.frame(InputDataset())
      }
      else{
        dt <- as.data.frame(InputDataset()[, c(input$SelectX)])
      }

    })


    output$Xvarselection <- renderUI({
      box(
        pickerInput("SelectX",
                    "Select variables:",
                    multiple = TRUE,
                    choices = as.character(names(InputDataset_model())),
                    selected=as.character(names(InputDataset_model())),
                    width="200px"),
        status = "primary",
        width = "300px",
        height = "200px",
        solidHeader = TRUE,
        title = "X variables")
    })

    #
    output$SelectY <-  renderUI({
      box(
        selectizeInput('SelectY',
                       "Select variable to predict:",
                       multiple = F,
                       choices = as.character(names(InputDataset())),
                       selected=1,
                       width="200px"),
        status = "primary",
        width = "300px",
        height = "200px",
        solidHeader = TRUE,
        title = "Y Variable")
    })


    # observe({
    #   lstname <- names(InputDataset())
    #   updateSelectInput(session = session,
    #                     inputId = "SelectY",
    #                     choices = lstname)
    # })

    splitSlider <- reactive({
      input$Slider1 / 100
    })
    output$Summ <-
      renderPrint(
        stargazer(
          InputDataset(),
          type = "text",
          title = "Descriptive statistics",
          digits = 1,
          out = "table1.txt"
        )
      )
    output$Summ_old <- renderPrint(summary(InputDataset()))
    output$structure <- renderPrint(str(InputDataset()))

    set.seed(100)  # setting seed to reproduce results of random sampling
    trainingRowIndex <-
      reactive({
        sample(1:nrow(InputDataset_model()),
               splitSlider() * nrow(InputDataset_model()))
      })# row indices for training data

    trainingData <- reactive({
      tmptraindt <- InputDataset_model()
      tmptraindt[trainingRowIndex(), ]
    })

    testData <- reactive({
      tmptestdt <- InputDataset_model()
      tmptestdt[-trainingRowIndex(),]
    })



    output$cntTrain <-
      renderText(paste("Train Data:", NROW(trainingData()), "records"))
    output$cntTest <-
      renderText(paste("Test Data:", NROW(testData()), "records"))

    output$Data <- DT::renderDT(InputDataset())

    # identify variables that are numeric for correlation matrix
    numeric_model_input <- reactive({
      nums <- unlist(lapply(InputDataset(), is.numeric))
      return(InputDataset()[ , nums])
    })

    cormat <- reactive({
      round(cor(numeric_model_input()), 1)
    })
    output$Corr <-
      renderPlot(corrplot(
        cormat(),
        type = "lower",
        order = "hclust",
        method = "number"
      ))

    correlationMatrix <- reactive({
      cor(numeric_model_input())
      print(cor(numeric_model_input()))
    })

    # it's this line that causes problems
    output$CorrMatrix <-
      renderPrint(round(as.data.frame(correlationMatrix()), 4))
    #





  print("At the end of the shiny server app")

}

