


# #' @import shinyalert
# #' @import shinydashboard
# #' @import shiny
# #' @importFrom DT
# #' @import leaflet
# #' @import here
# #' @import leaflet.extras
# #' @importFrom shinyjs reset
# #' @importFrom shinyjs alert
# #' @importFrom xfun file_ext
# #' @import methods
# These two are for table formatting and I may need them when I add the table back into the program
# Right now quoted out.
# #' @import pixiedust
# #' @import shinydust

#another table formatting one I may not need. Right now quoted out
# #' @import reactable

#This package writes datat to xlsx file extensions. I may want this for downloading the template?
# The readxl reads excel documents - I really don't think I need ths.
# #' @import writexl
# #' @import readxl


#R markdown document and colours and fonts and things.
# #' @importFrom bslib is_bs_theme

#collection of common functions, I don't think I use ths.
# #' @importFrom berryFunctions is.error

# I don't think I need this one any more. But I may
# #' @import htmltools

#I probably need this one
# #' @import ggplot2

# plotly Visualization of charts and graphs and figures


#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom dplyr %>%
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet leafletProxy
#' @importFrom leaflet.extras addFullscreenControl
#' @importFrom leaflet providers
#' @importFrom leaflet clearMarkers
#' @importFrom leaflet clearMarkerClusters
#' @importFrom leaflet clearPopups
#' @importFrom shinydashboard box
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard tabBox
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles shinyFileChoose
#' @importFrom shinyFiles parseFilePaths
#' @importFrom plyr rbind.fill
#' @importFrom plotly plotlyOutput
#' @importFrom plotly style
#' @importFrom plotly layout
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
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
#' @importFrom shiny updateCheckboxInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny validate
#' @importFrom shiny a
#' @importFrom shiny actionButton
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny checkboxGroupInput
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
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils zip
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @importFrom stats family
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats na.exclude
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats residuals
#' @importFrom shinycssloaders withSpinner

#another table formatting one I may not need. Right now quoted out
#' @importFrom reactable getReactableState
#' @importFrom reactable renderReactable
#' @importFrom reactable reactable
#' @importFrom reactable reactableTheme
#' @import RDML


shinyAppServer <- function(input, output, session) {

  #Get the initial working directory
  start_wd <- getwd()
  on.exit(setwd(start_wd))

  #################### Set and.or reset the metadata df ########################
  #Function to reset the MDMAPR Data frame to have no data
  setResetMdmaprDataFile <- function() {

print("setResetMdmaprDataFile - Begin ")

    #Set up the programs main data file
    expectedHeaders <- base::c(
      "projectID", "projectName", "projectRecordedBy", "projectCreationDate", "projectOwner", "projectEmailContact",
      "projectInstitutionID", "projectNotes", "projectContinent", "projectCountry", "projectState", "projectLocality",
      "siteID", "siteType", "siteDescription", "stationID", "stationName", "stationDecimalLongitude",
      "stationDecimalLatitude", "stationNotes", "replicateID", "replicateName", "replicateCollectorName",
      "replicateCollectionDate", "replicateCollectionTime", "replicateStorageID", "replicateStorageDate",
      "replicateStorageLocation", "replicateStorageMethod", "replicateMinEleInM", "replicateMaxEleInM",
      "replicateMinDepthInM", "replicateMaxDepthInM", "replicateFlowRateInMperS", "replicateFilterType",
      "replicateFiltDurationInS", "replicateVolume", "replicateProcessLocation", "replicateRiparianVegPerCover",
      "replicateDissO2InmgPerL", "replicateWaterTempInC", "replicatepH", "replicateTSSInMgperL", "replicateECInuSpercm",
      "replicateTurbInNTU", "replicateDischarge", "replicateTide", "replicateChlorophyl", "replicateSalinityInppt",
      "replicateContaminantsInngPerg", "replicatesTraceMetalsInmgPerkg", "replicatePercentOrganicContent",
      "replicateMircoActivity", "replicateGrainSize", "replicateNotes", "extractID", "extractName", "extractAnalystName",
      "extractDate", "extractTime", "extractLocation", "extractMethod", "extractMethodCitation", "extractNotes",
      "extractTubePlateID", "extractDNAStorageLocation", "extractMethodOfStorage", "extractQuantMethod",
      "extractConcInngPerul", "assayID", "assayName", "assayDescription", "assayOwnership", "assayCitation",
      "assayDate", "assayGeneTarget", "assayGeneSymbol", "assayPrimeR", "assayPrimeF", "assayProbe",
      "assayAmpliconLenInBP", "assayProbeFluorescentTag", "assayDyes", "assayQuencher", "assayProbeMods",
      "assayTaxonID", "assayKingdom", "assayPhylum", "assayClass", "assayOrder", "assayFamily", "assayGenus",
      "assaySubgenus", "assaySpecies", "assayCommonName", "assayOrganismScope", "assayEstabMeans", "resultRunID",
      "resultReactID", "resultRecordedBy", "resultDate", "resultTime", "resultPlatform", "resultMachineID",
      "resultWellLoc", "resultSampleType", "resultStdCurveID", "resultTemplateConcInCopy", "resultUserProvThres",
      "resultUserProvCq", "resultUserCopyNum", "resultUserProvLOD", "resultUserProvLOQ", "resultRxnVolInuL",
      "resultFwdPrimeBatch", "resultRevPrimeBatch", "resultNTPConcInngPeruL", "resultPrimeConcInngPeruL",
      "resultProbeConcInngPeruL", "resultMgConcInngPeruL", "resultPolymeraseBatch", "resultPolymeraseConcInngPeruL",
      "resultthermocyclerProtocol", "resultPCRNotes", "Cycle1", "Cycle2", "Cycle3", "Cycle4", "Cycle5", "Cycle6",
      "Cycle7", "Cycle8", "Cycle9", "Cycle10", "Cycle11", "Cycle12", "Cycle13", "Cycle14", "Cycle15", "Cycle16",
      "Cycle17", "Cycle18", "Cycle19", "Cycle20", "Cycle21", "Cycle22", "Cycle23", "Cycle24", "Cycle25", "Cycle26",
      "Cycle27", "Cycle28", "Cycle29", "Cycle30", "Cycle31", "Cycle32", "Cycle33", "Cycle34", "Cycle35", "Cycle36",
      "Cycle37", "Cycle38", "Cycle39", "Cycle40", "Cycle41", "Cycle42", "Cycle43", "Cycle44", "Cycle45", "Cycle46",
      "Cycle47", "Cycle48", "Cycle49", "Cycle50", "Cycle51", "Cycle52", "Cycle53", "Cycle54", "Cycle55", "Cycle56",
      "Cycle57", "Cycle58", "Cycle59", "Cycle60", "Cycle61", "Cycle62", "Cycle63", "Cycle64", "Cycle65", "Cycle66",
      "Cycle67", "Cycle68", "Cycle69", "Cycle70"
    )

    #Set up an empty dataframe
    mdmaprDataFile <- data.frame(matrix(ncol = length(expectedHeaders), nrow = 0))

    colnames(mdmaprDataFile) <- expectedHeaders

    print("setResetMdmaprDataFile - End ")
    # Return the empty dataframe
    return(mdmaprDataFile)

  }

  #########  Create a function for initial setting and resetting of  map #########
  setResetMap <- function() {
    #Building the initial map and using the
    #Default static leaflet map before filtering parameters are applied.
    output$mymap <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%

        #View map full screen (note: only works in web browser)
        leaflet.extras::addFullscreenControl() %>%

        #Default map view --> Change to Guelph
        leaflet::setView(lng = -80.2262, lat = 43.5327, zoom = 3) %>%

        #Change leaflet map tiles
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap)

    })
    leaflet::leafletProxy("mymap") %>%
      leaflet::clearMarkers() %>%
      leaflet::clearMarkerClusters() %>%
      leaflet::clearPopups()
  }

  ################################ Process RDML ##################################
  process_Multiplexed_RDML <- function (fdata) {

    print("process_Multiplexed_RDML - Begin")

    #Build a unique ID to mirror the metadata file unique ID
    fdata <- fdata[, base::c( "exp.id", "run.id", "react.id", "sample","target", "fluor")]

    #Remove all whitespaces
    #Apply gsub to remove all white spaces from specified columns
    fdata[,base::c(1,2,3,4,5)] <- lapply(fdata[,base::c(1,2,3,4,5)], function(col) {
      gsub("\\s", "", col)
    })

    #build the uniue ID
    built_unique_ID<-paste0(fdata$exp.id,"_",fdata$run.id,"_", fdata$react.id,"_", fdata$target,"_",fdata$sample)

    #Add the unique ID to the data frame
    fdata<-as.data.frame(cbind(built_unique_ID,fdata$fluor))

    # split by position
    formatted_df <- do.call('data.frame', split(fdata, fdata$built_unique_ID))

    # remove columns with just the well position
    formatted_df <- Filter(function(x)(length(unique(x))>1), formatted_df)

    # rename column with just the delimiter
    colnames(formatted_df) <- sub("\\..*", "", colnames(formatted_df))

    # ensure the well numbers and cycle are captured as the first row in the data frame
    formatted_df <- rbind(colnames(formatted_df), formatted_df)

    # rownames should match the appropriate data (resultWellLoc then cycle numbers)
    rownames(formatted_df) <- base::c("built_unique_ID", paste0("Cycle", 1:(nrow(formatted_df)-1)))

    # transpose the df such that each well is a row
    formatted_df <- as.data.frame(t(formatted_df))

    print("process_Multiplexed_RDML - End")
    return (formatted_df)
  }

  #################### Combine RDML and Metadata files Function #########################################
  combineReadInDataFiles <- function(totalMDMAPRDataFile, readInMetadata, readInqPCRData){

    #Combine the loaded in RDML and metadata
    # Get the column names from readInqPCRData except built_unique_ID
    update_columns <- setdiff(names(readInqPCRData), "built_unique_ID")

    # Merge readInMetadata and readInqPCRData on the built_unique_ID
    merged_df <- merge(readInMetadata, readInqPCRData, by = "built_unique_ID", all.x = TRUE)

    # Replace values in the specified columns
    for (col in update_columns) {
      merged_df[[paste0(col, ".x")]] <- ifelse(!is.na(merged_df[[paste0(col, ".y")]]), merged_df[[paste0(col, ".y")]], merged_df[[paste0(col, ".x")]])
    }

    # Drop the auxiliary columns from merged_df
    totalMDMAPRDataFile <- merged_df[, !names(merged_df) %in% paste0(update_columns, ".y")]

    # Rename the columns back to their original names
    names(totalMDMAPRDataFile) <- sub("\\.x$", "", names(totalMDMAPRDataFile))

    return(totalMDMAPRDataFile)

  }

  #################### Replace columns in the dataframe ##########################
  # Function to replace columns in one data frame with columns from another data frame
  # This function accepts a dataframe with unique_built_ID as row names and then
  # columns are the ones to add to the main data frame
  replace_columns <- function(df1, df2, cols) {

    print("Begin replace_columns")

    # Check if there are missing columns in the main data frame and if there are
    # add in the columns at the end
    for (col in cols) {
      if (!col %in% colnames(df1)) {
        df1[[col]] <- NA
      }
    }

    if (ncol(df2) == 0) {
      print("End replace_columns")
      return(df1)
    }else{

      # Replace entries in df1 with those from df2 where row and column names match
      for (row in rownames(df2)) {
        if (row %in% rownames(df1)) {
          for (col in colnames(df2)) {
            if (col %in% colnames(df1)) {
              df1[row, col] <- df2[row, col]
            }
          }
        }
      }
      print("End replace_columns")
      return(df1)
    }
  }

  ############### Second Derivative Threshold Calculation ########################
  calculate_second_deriv_threshold <- function(fluorescence_values_df){

    print("Begin calculate_second_deriv_threshold")

    #initialize the output vector
    threshResult <- base::c()

    for (sampleCount in 1:nrow(fluorescence_values_df)){

      #Get the row of interest
      cleaned_data <- fluorescence_values_df[sampleCount,,drop = FALSE]

      # Remove columns with NA values
      cleaned_data <- cleaned_data[, colSums(is.na(cleaned_data)) == 0]

      if(ncol(cleaned_data)!=0){

        # Check for numeric values and remove non-numeric columns
        is_numeric <- sapply(cleaned_data, function(x) !is.na(as.numeric(x)))
        cleaned_data <- cleaned_data[, is_numeric]

        #Get the total number of thremocycler cycles for this sample
        number_of_cycles <- ncol(cleaned_data)

        #Set the initial absorbance cycle to the cycle with the minimum absorbance value
        place_holder_cycle = which.min(cleaned_data)

        #Step through the data points for the cycles from the minimum value to the end of the cycles to see if the data is increasing within a certain percentage as compared to the total absorbance range for the sample
        exp_phase_subset <- base::c()

        for (cycle_index in place_holder_cycle:(length(cleaned_data)-1)) {
          # calculate the absolute value difference between that cycle and the following cycle
          absolute <- abs(as.numeric(cleaned_data[1,cycle_index]) - as.numeric(cleaned_data[1,cycle_index+1]))
          maximum <- max(as.numeric(cleaned_data[1,cycle_index]),as.numeric(cleaned_data[1,cycle_index+1]))
          if((absolute/maximum) >= 0.025) {
            # this vector will contain the cycle number index that meet the defined criteria
            exp_phase_subset <- base::c(exp_phase_subset, cycle_index)
          }
        }

        #Calculate the mdmaprThres
        if (is.null(exp_phase_subset)|length(exp_phase_subset)<2){
          threshResult <- base::c(threshResult,"Unable to compute")
        }else{
          # update the place holder cycle to the minimum of this subset
          place_holder_cycle <- min(exp_phase_subset)

          # create the data frame with the absorbance, cycle in columns
          data_to_plot <- data.frame(Fluorescence = as.vector(t(cleaned_data[1, exp_phase_subset])), Cycle = as.vector(exp_phase_subset))

          # Add a row with values 0,0 to the beginning of the data frame
          new_row <- data.frame(Cycle = 0, Fluorescence = 0)
          data_to_plot <- rbind(new_row, data_to_plot)

          #ensure that they are all numeric
          data_to_plot <- as.data.frame(lapply(data_to_plot, function(x) as.numeric(as.character(x))))

          # Calculate first derivative (ΔRFU)
          data_to_plot$First_Derivative <- base::c(diff(data_to_plot$Fluorescence) / diff(data_to_plot$Cycle), NA)

          # Calculate second derivative (ΔΔRFU)
          #NOTE the way that the calculation was set up the original cycles will have a second deravitive value
          #except for the first and the last cycle being analysed.
          data_to_plot$Second_Derivative <- base::c(NA,diff(data_to_plot$First_Derivative) / diff(data_to_plot$Cycle))

          # Find the maximum value of the second derivative
          max_second_derivative_value <- max(data_to_plot$Second_Derivative, na.rm = TRUE)

          # Get all rows with the maximum second derivative
          data_to_plot <- data_to_plot[data_to_plot$Second_Derivative == max_second_derivative_value, ,drop=FALSE]

          #Remove any second derivative NA's if present
          data_to_plot <- data_to_plot[!is.na(data_to_plot[, ncol(data_to_plot)]), ]

          #The threshold value at the cycle determined by the double derivitave
          threshold_value <- data_to_plot$Fluorescence[1]

          #Add threshold value to dataframe
          threshResult <- base::c(threshResult,threshold_value)
        }
      }else{
        threshResult <- base::c(threshResult,"Unable to compute")
      }

    }

    fluorescence_values_df <- cbind(fluorescence_values_df, mdmaprThres =  threshResult)

    print("End calculate_second_deriv_threshold")

    return(fluorescence_values_df)

  }

  ###################### Calculating the MDMAPR Cq Values ########################
  #This takes a dataframe as an input with the ID for the first column and the absorbance
  #values for the rest
  mdmaprCqCalc <- function(raw_multiplex_data, thresholdColName, cqOutName) {

    print("Begin mdmaprCqCalc")

    #subset the totalMDMAPRDataFile$value to only keep the Cycle columns
    fluorescence_data <- raw_multiplex_data[, base::c(grep("Cycle", names(raw_multiplex_data), value = TRUE))]

    #initialize the output vector
    cqResults <- base::c()

    for (sampleCount in 1:nrow(fluorescence_data)){

      if(is.na(suppressWarnings(as.numeric(raw_multiplex_data[[thresholdColName]][sampleCount])))){
        cqResults <- base::c(cqResults,"Unable to compute")
      }else{

        #Remove all of the NA values which will be present in the Cycles that were not used.
        cleanedRow <- fluorescence_data[sampleCount, colSums(is.na(fluorescence_data[sampleCount,])) == 0, drop = FALSE]

        if(ncol(cleanedRow)==0){

          cqResults <- base::c(cqResults,"Unable to compute")

        }else{

          if(max(as.numeric(cleanedRow))<as.numeric(raw_multiplex_data[[thresholdColName]][sampleCount])){
            cqResults <- base::c(cqResults,"Unable to compute")
          }else{

            #Get the minimum cycle position where the absorbance is larger than the threshold
            cycleCqResult <-min(which(as.numeric(cleanedRow) > as.numeric(raw_multiplex_data[[thresholdColName]][sampleCount])))

            #Take away one from the calculated cQ because this would represent the cycle that worked
            cycleCqResult-1

            cqResults <- base::c(cqResults, cycleCqResult)

          }
        }
      }
    }

    #Add the mdmaprCq values on to the dataframe
    raw_multiplex_data<-cbind(raw_multiplex_data,cqResults)

    #Rename the last column to the desired name based on the submitted values.
    names(raw_multiplex_data)[ncol(raw_multiplex_data)] <- cqOutName

    print("End mdmaprCqCalc")

    return(raw_multiplex_data)

  }

  ######################## LOD and LOQ calculations ##############################
  # Function that calculates LOD and LOQ for the standard curve data using the generic method
  calculate_SC_LOD_LOQ <- function(merged_file, threshold, detectRate, cycles){

    merged_fileLODLOQ<<-merged_file
    thresholdLODLOQ <<- threshold
    detectRateLODLOQ <<- detectRate
    cyclesLODLOQ <<- cycles

print("calculate_SC_LOD_LOQ - Begin")

    #Rename the second column to Cq so I can use the column name in the linear regression later.
    colnames(merged_file)[2] <- "Cq"

    #Rename the third column to Conc so I can use the column name in the linear regression later.
    colnames(merged_file)[3] <- "Conc"

    #Remove rows with no Cq values by converting the second column to numeric, coercing non-numeric values to NA
    merged_file$Log10_Concentration <- suppressWarnings(as.numeric(merged_file$Cq))

    # Remove rows with NA values in the second column
    merged_fileTemp <- merged_file[!is.na(merged_file$Log10_Concentration), ]

    # Calculate the log10 of concentrations
    merged_fileTemp$Log10_Concentration <- log10(as.numeric(merged_fileTemp$Conc))

    # Replace -Inf with NA and then lm() will ignore NA
    merged_fileTemp$Log10_Concentration[merged_fileTemp$Log10_Concentration == -Inf] <- NA

    # Fit a linear model
    lm_model <- stats::lm(Cq ~ Log10_Concentration, data = merged_fileTemp)

    # Get the summary of the linear model
    summary(lm_model)

    # Calculate the efficiency of the PCR standard curve
    slope <- stats::coef(lm_model)[2]

    # Calculate efficiency
    efficiency <- 10^(-1/slope) - 1

    # Generate the equation of the line and the r squared value using the model data in lm_model
    equation <- paste("y =",   round(stats::coef(lm_model)[2], 2), "x", "+",round(stats::coef(lm_model)[1], 2))
    r_squared <- round(summary(lm_model)$r.squared, 2)

    # Step 1: Change all Cq with the max qPCR cycle to NA
    merged_file$Cq[which(merged_file$Cq==cycles)] <- NA

    # Set the values in the columns to numeric
    merged_file$Cq <- suppressWarnings(as.numeric(merged_file$Cq))
    merged_file$resultTemplateConcInCopy <- as.numeric(merged_file$Conc)
    merged_file$Log10_Concentration <- as.numeric(merged_file$Log10_Concentration)

    # Step 2: Determine the dilutions used in the standard curve
    Standards <- unique(merged_file$resultTemplateConcInCopy[!is.na(merged_file$resultTemplateConcInCopy)])

    # Step 3: Calculate the rate of detection for each standard (any well with a Cq value is considered a detection)
    # Calculate other metrics for LOQ calculations (Cq mean, standard deviation, coefficient of variation)
    DAT2 <- data.frame(Standards=Standards,Reps=NA,Detects=NA,Cq.mean=NA,
                       Cq.sd=NA,Cq.CV=NA)

    for(i in 1:nrow(DAT2)) {
      #initialize the data columns with the number of values per concentration
      DAT2$Reps[i] <- sum(merged_file$resultTemplateConcInCopy==DAT2$Standards[i],na.rm=TRUE)
      #Determine the detections
      DAT2$Detects[i] <- sum(!is.na(merged_file$Cq)&merged_file$resultTemplateConcInCopy==DAT2$Standards[i],na.rm=TRUE)
      #Calculate the detection rate at each of the concentrations
      DAT2$DetectionRate[i] <- DAT2$Detects[i]/DAT2$Reps[i]
      #Calculate the average for the values at each concentration
      DAT2$Cq.mean[i] <- mean(merged_file$Cq[merged_file$resultTemplateConcInCopy==DAT2$Standards[i]],na.rm=TRUE)
      #Calculate the standard deviation for the values at each concentration
      DAT2$Cq.sd[i] <- stats::sd(merged_file$Cq[merged_file$resultTemplateConcInCopy==DAT2$Standards[i]],na.rm=TRUE)
      #Calculate the coefficient of variation
      DAT2$Cq.CV[i] <- sqrt(2^(DAT2$Cq.sd[i]^2*log(2))-1)
    }

    # Step 4: Determine the lowest concentration with at least a 95% detection rate
    # we want to assess whether a concentration above the LOD has greater variation

    #Determine the LOD
    LOD <- min(DAT2$Standards[DAT2$DetectionRate>=detectRate])

    if(length(which(DAT2$DetectionRate<detectRate))>0) {
      B <- max(DAT2$Standards[DAT2$DetectionRate<detectRate])
      if(B>LOD) {
        warning <- paste0("WARNING: For ",merged_file$list_of_curves[1],", ",B," copies/reaction standard detected at lower rate than ",LOD," copies/reaction standard.Please retest.")
      }
    }

    if(length(which(DAT2$DetectionRate<detectRate))==0) {
      warning <- paste0("WARNING: LoD cannot be determined for ",merged_file$list_of_curves[1],", because it is lower than the lowest standard you tested. Report as <",LOD," copies/reaction, or retest with lower concentrations.")
    }

    # Step 5: Find the LOQ value (lowest standard concentration that is above the LOD
    # that can be quantified with a CV value below the defined threshold).
    # Default CV value will be 35%.
    DATtemp <- DAT2[DAT2$Standards >= LOD,, drop = TRUE]
    LOQ <- min(DATtemp$Standards[DATtemp$Cq.CV<as.numeric(threshold)], na.rm = TRUE)

    #Drop all columns in the data frame except the unique identifiers column
    merged_file <- merged_file[,1,drop=FALSE]

    # Generate the equation of the line and the r squared value using the model data in lm_model
    merged_file$equation <- equation
    merged_file$efficiency <- efficiency
    merged_file$rsq <- r_squared
    merged_file$LOD <- LOD
    merged_file$LOQ <- LOQ

    print("calculate_SC_LOD_LOQ - End")

    return(merged_file)
  }

  ############################# Concentration Calculations #######################
  # Function to process data frame, calculate x values, and apply reverse log transformation
  conc_calc <- function(df) {

    print("Begin conc_calc")

    calculate_x <- function(equation_string, y_value) {
      # Define a function to extract slope and intercept
      extract_slope_intercept <- function(equation_string) {
        pattern <- "y = ([+-]?\\d*\\.?\\d+) x \\+ ([+-]?\\d*\\.?\\d+)"
        matches <- regmatches(equation_string, regexec(pattern, equation_string))
        if (length(matches[[1]]) == 0) {
          return(list(valid = FALSE))
        } else {
          slope <- as.numeric(matches[[1]][2])
          intercept <- as.numeric(matches[[1]][3])
          return(list(valid = TRUE, slope = slope, intercept = intercept))
        }
      }

      # Extract slope and intercept
      extracted <- extract_slope_intercept(equation_string)
      if (!extracted$valid) {
        return("Unable to compute")
      }

      # Calculate x value
      y_value <- suppressWarnings(as.numeric(y_value))
      if (!is.na(y_value)) {
        slope <- extracted$slope
        intercept <- extracted$intercept
        x_value <- (y_value - intercept) / slope
        if (!is.na(x_value)) {
          return(x_value)  # Return 10^x_value for reverse log transformation
        } else {
          return("Unable to compute")
        }
      } else {
        return("Unable to compute")
      }
    }

    # Initialize result vector
    x_values <- vector("list", nrow(df))

    # Iterate over each row of the data frame
    for (i in seq_len(nrow(df))) {
      equation_string <- df[i, 1]
      y_value <- df[i, 2]
      x_values[[i]] <- calculate_x(equation_string, y_value)
    }

    # Create a single-column data frame with the result
    result_df <- data.frame(Transformed_X = unlist(x_values))

    print("End conc_calc")

    return(result_df)
  }

  ############################ Obtain the log linear region function ###########################
  #Log linear calc function
  logLinear <- function(cqDataTable, CqHeader){

    print("Begin logLinear")

    #Define the R squared log linear cutoff
    RSquaredCutoff = 0.99
    #Set up the vector to hold the results
    logLinear <- base::c()

    #so loop through all of the records in the data set.
    for(logLinearLoopCounter in 1:nrow(cqDataTable)){

      #Get the Cq Value for this record and set it to a numeric value or NA
      CqValue <- suppressWarnings(as.numeric(cqDataTable[[logLinearLoopCounter,CqHeader]]))

      #Check to see if the CqHeader column had a numeric value if it doesn't then
      # set the efficiency to Unable to compute
      if(!is.na(CqValue)){
        #subset the cqDataTable to only keep the Cycle columns of interest and only start at the Cq value
        raw_multiplex_data <- cqDataTable[logLinearLoopCounter, base::c(grep("Cycle", names(cqDataTable), value = TRUE))]
        raw_multiplex_data <- raw_multiplex_data[,CqValue:length(raw_multiplex_data)]
        raw_multiplex_data <- na.omit(as.vector(unlist(raw_multiplex_data)))

        for (logLinearEndCounter in 1:(length(raw_multiplex_data))) {

          if(length(raw_multiplex_data)-logLinearEndCounter >1){

            # Current values without the last i elements
            current_values <- raw_multiplex_data[1:(length(raw_multiplex_data)-logLinearEndCounter)]

            # Create a vector of numbers from 1 to the length of the current_values
            numbers <- 1:length(current_values)

            # Create a data frame with the current_values and numbers
            df <- data.frame(Number = numbers, Value = current_values)

            fit <- lm(log10(as.numeric(df$Value)) ~ as.numeric(df$Number))
            r_squared <- summary(fit)$r.squared

            # print(paste0("R squared - ", r_squared, " length cycle - ", length(current_values)))
            if(r_squared >= RSquaredCutoff){

              logLinearEnd = CqValue + length(raw_multiplex_data) - logLinearEndCounter

              #Add the results from this loop
              loopResult <- paste0(floor(CqValue),":",ceiling(logLinearEnd))
              logLinear <- base::c(logLinear,loopResult)
              break
            }

          }else{
            logLinear <- base::c(logLinear,"Unable to compute")
            break
          }
        }
      }else{
        logLinear <- base::c(logLinear,"Unable to compute")
      }#End of if else
    }#End of loop
    logLinearResults <- data.frame(logLinear)
    return(logLinearResults)
  }#End of PCR efficiency function

  ############### Given the equation of a line solve for X function ############
  # Function to solve for x in the equation y = m * x + b
  solve_for_x <- function(equation, y_value) {

    # Extract the numeric values using regular expressions
    matches <- regmatches(equation, gregexpr("[-]?\\d+\\.\\d+", equation))

    # Convert the extracted matches to numeric format
    coefs <- as.numeric(unlist(matches))

    # Extract the slope and intercept
    slope <- as.numeric(coefs[1])
    intercept <- as.numeric(coefs[2])

    # Solve for x
    x <- (y_value - intercept) / slope

    return(x)
  }


  #################### Data Calculations Function #########################################
  complete_calc <- function(){

    shiny::showModal(shiny::modalDialog(
      title = "Processing, please stand by...",
      footer=""

    ))

    print("Begin complete_calc")

    # confirm built_unique_ID is the row name
    row.names(totalMDMAPRDataFile$value)<- as.character(totalMDMAPRDataFile$value$built_unique_ID)

    ######################## user supplied Threshold values Cq calcs ###############

    #subset the totalMDMAPRDataFile$value to only keep the Cycle columns
    raw_multiplex_data <- totalMDMAPRDataFile$value[, base::c(grep("Cycle", names(totalMDMAPRDataFile$value), value = TRUE))]

    #Add on the user supplied threshold values
    raw_multiplex_data<-cbind(raw_multiplex_data, resultUserProvThres = totalMDMAPRDataFile$value$resultUserProvThres)

    #Calculate the mdmaprCq values
    raw_multiplex_data <- mdmaprCqCalc(raw_multiplex_data, "resultUserProvThres", "mdmaprCqwUserThres")

    #Only keep the last column to send to the add row function
    raw_multiplex_data<-raw_multiplex_data[,ncol(raw_multiplex_data), drop = FALSE]

    #Add the mdmaprCqwUserThres values to the totalMDMAPRDataFile
    totalMDMAPRDataFile$value <- replace_columns(totalMDMAPRDataFile$value, raw_multiplex_data, base::c("mdmaprCqwUserThres"))

    ######################## mdmapr Threshold values Cq calcs ##################

    #subset the totalMDMAPRDataFile$value to only keep the Cycle columns
    raw_multiplex_data <- totalMDMAPRDataFile$value[, base::c(grep("Cycle", names(totalMDMAPRDataFile$value), value = TRUE))]

    # calculate threshold NOTE: Currently the calculate_second_deriv_threshold allows a dip in absorbance of 2.5%
    #when assessing the beginning of real data to calculate the threshold.
    raw_multiplex_data <- calculate_second_deriv_threshold(raw_multiplex_data)

    #Calculate the mdmaprCq values
    raw_multiplex_data <- mdmaprCqCalc(raw_multiplex_data, "mdmaprThres", "mdmaprCq")

    #Add the mdmaprThres and mdmaprCq values to the totalMDMAPRDataFile
    totalMDMAPRDataFile$value <- replace_columns(totalMDMAPRDataFile$value, raw_multiplex_data, base::c("mdmaprThres", "mdmaprCq"))

    ################## PCR Log Linear ##########################################

    #Run the logLinear function and get the efficienceies of the PCR's
    logLinearOut <- logLinear(totalMDMAPRDataFile$value, "mdmaprCq")

    #Add the results from the PCR efficiency back to the main data frame
    raw_multiplex_data<-cbind(totalMDMAPRDataFile$value, mdmaprLogLinear = logLinearOut[,1])

    #Add the values to the totalMDMAPRDataFile
    totalMDMAPRDataFile$value <- replace_columns(totalMDMAPRDataFile$value, raw_multiplex_data, base::c("mdmaprLogLinear"))

    #Run the PCR_Eff function and get the efficienceies of the PCR's
    logLinearOut <- logLinear(totalMDMAPRDataFile$value, "resultUserProvCq")

    #Add the results from the PCR efficiency back to the main data frame
    raw_multiplex_data<-cbind(totalMDMAPRDataFile$value, userLogLinear = logLinearOut[,1])

    #Add the values to the totalMDMAPRDataFile
    totalMDMAPRDataFile$value <- replace_columns(totalMDMAPRDataFile$value, raw_multiplex_data, base::c("userLogLinear"))

    #################### Standard Curve Data prep ##################################

    #Get records that have opt and are therefore designated as standard curve results
    stdCurveData<-totalMDMAPRDataFile$value[totalMDMAPRDataFile$value$resultSampleType == "opt" | totalMDMAPRDataFile$value$resultSampleType == "ntc",, drop=FALSE]

    #Calculate std curve values using the 35% as the Klymus method also uses (Could add elow quant to this later)
    #first see if there are records remaining and then see if there are number values in the first cycle
    if(nrow(stdCurveData)>0){

      #Get the unique standard curve datasets
      list_of_curves <- unique(stdCurveData$resultStdCurveID)

      #Loop through the different standard curves
      for (list_of_curves_counter in 1:length(list_of_curves)){

        #Subset the stdCurveData table for the curve for this loop
        stdCurveDataLoopCurve<-stdCurveData[stdCurveData$resultStdCurveID == list_of_curves[list_of_curves_counter],]

        #Pull all relevant data to check if there were multiple instances of running
        #the same standard curve
        #Get the unique standard curve datasets
        list_of_runs <- unique(stdCurveData$resultRunID)

        #Loop through the different standard curves
        for (list_of_runs_counter in 1:length(list_of_runs)){

          threshold<-0.35 #LOQ value
          detectRate<-0.95 #LOD value

          #Subset the stdCurveDataLoopCurve table for the curve for this loop
          stdCurveDataLoopCurveReaction<-stdCurveDataLoopCurve[stdCurveDataLoopCurve$resultRunID == list_of_runs[list_of_runs_counter],]

          #Get the total number of cycles used for the standard curve first select only cycle columns
          cycles <- stdCurveDataLoopCurveReaction[,grep("Cycle", names(stdCurveDataLoopCurveReaction), value = TRUE)]
          #Remove columns with only NA values
          cycles <- cycles[,colSums(is.na(cycles))<nrow(cycles)]
          #count the number of columns to obtain the number of cycles
          cycles <- ncol(cycles)

          # Getting the LOD and LOQ for user values
          submissionData<-stdCurveDataLoopCurveReaction[, base::c("built_unique_ID", "resultUserProvCq", "resultTemplateConcInCopy")]

          #Call the calculate_SC_LOD_LOQ function using the subset data.
          formatted_metadata_SC_Calc_loop_curve<-calculate_SC_LOD_LOQ(submissionData, threshold, detectRate, cycles)

          #update the names of the column for the submitted data. The returned values are
          # column 2 - equation,3 - Rsq, 4 - LOD, 5 - LOQ
          names(formatted_metadata_SC_Calc_loop_curve)[2] <- "userStdCurveLineEq"
          names(formatted_metadata_SC_Calc_loop_curve)[3] <- "userStdCurveEfficiency"
          names(formatted_metadata_SC_Calc_loop_curve)[4] <- "userStdCurveRSq"
          names(formatted_metadata_SC_Calc_loop_curve)[5] <- paste0("userStdCurveLOD",(detectRate*100))
          names(formatted_metadata_SC_Calc_loop_curve)[6] <- paste0("userStdCurveLOQ",(threshold*100))

          #Add the userStdCurveLineEq, userStdCurveEfficiency, userStdCurveRSq, userStdCurveLOD95, userStdCurveLOQ35" values to the totalMDMAPRDataFile
          totalMDMAPRDataFile$value <- replace_columns(totalMDMAPRDataFile$value, formatted_metadata_SC_Calc_loop_curve, base::c("userStdCurveLineEq", "userStdCurveEfficiency", "userStdCurveRSq", "userStdCurveLOD95", "userStdCurveLOQ35"))

          ######################### Now the mdmapr values ######################

          # Getting the LOD and LOQ for the standard curves for mdmapr values
          submissionData<-stdCurveDataLoopCurveReaction[, base::c("built_unique_ID", "mdmaprCq", "resultTemplateConcInCopy")]

          #Call the calculate_SC_LOD_LOQ function using the subset data.
          formatted_metadata_SC_Calc_loop_curve<-calculate_SC_LOD_LOQ(submissionData, threshold, detectRate, cycles)

          #update the names of the column for the submitted data. The returned values are
          # column 2 - equation,3 - Rsq, 4 - LOD, 5 - LOQ
          names(formatted_metadata_SC_Calc_loop_curve)[2] <- "mdmaprStdCurveLineEq"
          names(formatted_metadata_SC_Calc_loop_curve)[3] <- "mdmaprStdCurveEfficiency"
          names(formatted_metadata_SC_Calc_loop_curve)[4] <- "mdmaprStdCurveRSq"
          names(formatted_metadata_SC_Calc_loop_curve)[5] <- paste0("mdmaprStdCurveLOD",(detectRate*100))
          names(formatted_metadata_SC_Calc_loop_curve)[6] <- paste0("mdmaprStdCurveLOQ",(threshold*100))

          #Add the mdmaprStdCurveLineEq, mdmaprStdCurveEfficiency, mdmaprStdCurveRSq, mdmaprStdCurveLOD95, mdmaprStdCurveLOQ35" values to the totalMDMAPRDataFile
          totalMDMAPRDataFile$value <- replace_columns(totalMDMAPRDataFile$value, formatted_metadata_SC_Calc_loop_curve, base::c("mdmaprStdCurveLineEq", "mdmaprStdCurveEfficiency", "mdmaprStdCurveRSq", "mdmaprStdCurveLOD95", "mdmaprStdCurveLOQ35"))

        }

      }#End of loop through standard curve data

      ######################### Add LOD and LOQ to all records ######################
      #Add LOD and LOQ to all records if there is an associated standard curve to the
      #records present.

      # Define the substrings to search for
      colIdnetifiers <- base::c("resultStdCurveID", "StdCurveLineEq", "StdCurveEfficiency", "StdCurveRSq", "StdCurveLOD", "StdCurveLOQ")

      # Get the column numbers that contain any of the colIdnetifiers
      col_numbers <- grep(paste(colIdnetifiers, collapse="|"), names(totalMDMAPRDataFile$value))

      #Get the columns of interest
      stdCurveTable <- unique(totalMDMAPRDataFile$value[,base::c(col_numbers)])

      # Remove rows with NA in columns 2 through last column
      stdCurveTable <- stdCurveTable[stats::complete.cases(stdCurveTable[, 2:ncol(stdCurveTable)]), ,drop=FALSE]

      #Loop thorough each of the resultStdCurveID variables.
      for (loopAssayID in nrow(stdCurveTable)){

        #Keep only the records where there is assay data for this loop
        loopAssayIDRecords<- totalMDMAPRDataFile$value[totalMDMAPRDataFile$value$resultStdCurveID == stdCurveTable[[loopAssayID,"resultStdCurveID"]],,drop=FALSE]

        # Identify columns in stdCurveTable that are also in loopAssayIDRecords
        cols_to_update <- intersect(names(loopAssayIDRecords), names(stdCurveTable)[-1])

        # Update all rows in df1 with values from df2 for matching columns
        loopAssayIDRecords[, cols_to_update] <- stdCurveTable[rep(1, nrow(loopAssayIDRecords)), cols_to_update]

        #using the replace columns function update the totalMDMAPRDataFile$value using the loopAssayIDRecords
        totalMDMAPRDataFile$value<-replace_columns(totalMDMAPRDataFile$value,loopAssayIDRecords,names(stdCurveTable)[-1])

      }#End of loop going through assay standard curves

    }# end of if std curve data

    ############## Calculate the copy number using standard curve data #########

    # Call the concentration calculation function and submit a two column dataframe
    # w the Cq value and the equation of the line to calculate the concentration
    # based on the standard curve

    #Get the column numbers of interest
    column_numbers <- base::c(which(names(totalMDMAPRDataFile$value) %in% base::c("userStdCurveLineEq")),
                              which(names(totalMDMAPRDataFile$value) %in% base::c("resultUserProvCq")))

    #First get the dataframe of values for analysis
    concToCalc <- totalMDMAPRDataFile$value[,base::c(column_numbers),drop=FALSE]

    #Use the conc_calc() function to calculate the concentration of the samples
    sampleCopyNumber <- as.data.frame(conc_calc(concToCalc))
    colnames(sampleCopyNumber) <-base::c("userCopyNum")

    #Add the copy number to the data frame
    totalMDMAPRDataFile$value <- cbind(totalMDMAPRDataFile$value, as.data.frame(sampleCopyNumber))

    #Get the column numbers of interest
    column_numbers <- base::c(which(names(totalMDMAPRDataFile$value) %in% base::c("mdmaprStdCurveLineEq")),
                              which(names(totalMDMAPRDataFile$value) %in% base::c("mdmaprCq")))

    #First get the dataframe of values for analysis
    concToCalc <- totalMDMAPRDataFile$value[,base::c(column_numbers),drop=FALSE]

    #Use the conc_calc() function to calculate the concentration of the samples
    sampleCopyNumber <- as.data.frame(conc_calc(concToCalc))
    colnames(sampleCopyNumber) <-base::c("mdmaprCopyNum")

    #Add the copy number to the data frame
    totalMDMAPRDataFile$value <- cbind(totalMDMAPRDataFile$value, as.data.frame(sampleCopyNumber))

    removeModal()
    shiny::showModal(shiny::modalDialog(
      title = "Data processing is complete...",
    ))

print("End complete_calc")

  }#End of the Complete Calculations function

  ################# Calc Cq Intensity bins for mapping #########################
  mapIntensityBins <- function(values) {

    print("Begin mapIntensityBins")

    # Create vectors to store the results
    binned_results <- vector("character", length(values))
    range_labels <- vector("character", length(values))

    # Convert to numeric and identify non-numeric entries
    numeric_data <- suppressWarnings(as.numeric(values))
    is_numeric <- !is.na(numeric_data)

    # Check if there is enough numeric data to bin
    if (sum(is_numeric) == 0) {
      stop("No numeric data to bin")
    }

    # Find the range of the numeric data
    min_val <- min(numeric_data[is_numeric])
    max_val <- max(numeric_data[is_numeric])

    # Calculate the bin edges
    bin_edges <- seq(from = 0, to = max_val, length.out = 6) # 5 bins -> 6 edges

    # Bin the numeric data
    binned_data <- cut(numeric_data[is_numeric], breaks = bin_edges, include.lowest = TRUE, labels = FALSE)

    # Check if any values have decimal parts greater than zero
    if (any(bin_edges %% 1 > 0)) {
      # Make all of the decimals only have two places
      bin_edges <- as.numeric(sprintf("%.2f", bin_edges))

      # Create range labels for each bin with a 0.01 difference between end of one and start of the next
      bin_labels <- sapply(1:(length(bin_edges) - 1), function(i) {
        paste0(bin_edges[i], " to ", sprintf("%.2f", bin_edges[i + 1] - 0.01))
      })
    } else {
      # Create range labels for each bin with a 1 difference between end of one and start of the next
      bin_labels <- sapply(1:(length(bin_edges) - 1), function(i) {
        paste0(bin_edges[i], " to ", bin_edges[i + 1] - 1)
      })
    }

    # Populate the results with binned data, range labels, and "Unable to Compute" for non-numeric entries
    binned_results[is_numeric] <- as.character(binned_data)
    range_labels[is_numeric] <- bin_labels[binned_data]
    binned_results[!is_numeric] <- "Unable to Compute"
    range_labels[!is_numeric] <- "Unable to Compute"

    # Create the final data frame
    result_df <- data.frame(original = values, binned = binned_results, range = range_labels)

    # Get a sorted list of unique range labels
    sorted_range_labels <- unique(bin_labels)
    numeric_sort_key <- sapply(strsplit(sorted_range_labels, " to "), function(x) as.numeric(x[1]))
    sorted_range_labels <- sorted_range_labels[order(numeric_sort_key)]

    print("End mapIntensityBins")
    return(list(binned_data = result_df, sorted_range_labels = sorted_range_labels))

  }

  #################### Mapping the filtered data ###############################
  setMappingDataPoints <- function(filtered, mappedValueVal) {

print(paste0("In the set mapping points mappedValueVal = ", mappedValueVal))

    #Reset the map before updating the data points and the legend
    setResetMap()

    #Ensuring the filtered data is in a dataframe
    filtered<-as.data.frame(filtered)


    if(mappedValueVal == "resultUserProvCq"){ #End of the type of points plotted

print("In resultUserProvCq")

      mapPointIntensity = mapIntensityBins(filtered$resultUserProvCq)

      #Plotting the data points
      leaflet::leafletProxy("mymap", data = filtered) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearPopups() %>%
        #Adding labels to markers on map
        leaflet::addCircleMarkers(lng = ~as.numeric(stationDecimalLongitude),
                                  lat = ~as.numeric(stationDecimalLatitude),
                                  color = ~suppressWarnings(leaflet::colorFactor(
                                    palette = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
                                    domain = base::c(1, 2, 3, 4, 5))(mapPointIntensity$binned_data$binned)),
                                  options = leaflet::leafletOptions(zoomDelta = 0), # Disable zooming
                                  clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                                  popup= suppressWarnings(paste("<strong><h5>", filtered$assayGenus," ", filtered$assaySpecies, "</strong>",
                                                                "<br><h6>Assay|Marker:", filtered$assayName, "|",filtered$assayGeneTarget,
                                                                "<br><h6>Project|Site|Station|Replicate:", filtered$projectID,"|",filtered$siteID,"|",filtered$stationID,"|",filtered$replicateID,
                                                                "<br><h6>Extract|Assay|Run|Reaction:", as.character(filtered$extractID),"|",as.character(filtered$assayID),"|",as.character(filtered$resultRunID),"|",as.character(filtered$resultReactID),
                                                                "<br><h6>Collection Date:",filtered$replicateCollectionDate,
                                                                "<strong><h5>Thres.|Cq|Copy|Eff.:", as.numeric(sprintf("%.2f", as.numeric(filtered$resultUserProvThres))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$resultUserProvCq))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$userCopyNum))),"|",as.numeric(sprintf("%.2f",as.numeric(filtered$userPCREfficiency))),"</strong>",
                                                                "<br><h6>Type|Std Curve:", filtered$resultSampleType,"|",filtered$resultStdCurveID,
                                                                "<h6>Lab|Collector|Analyst|Platform:", filtered$projectInstitutionID,"|",filtered$replicateCollectorName,"|",filtered$extractAnalystName, "|", filtered$resultPlatform,
                                                                "<h6>Coord(Lat, Lon):", filtered$stationDecimalLatitude,",", filtered$stationDecimalLongitude))) %>%
        leaflet::addLegend(
          position = "topright",
          colors = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
          labels = mapPointIntensity$sorted_range_labels,
          title = 'resultUserProvCq Intensity',
          opacity = 0.6
        )#end of legend

    } else if(mappedValueVal == "resultUserCopyNum"){

print("In resultUserCopyNum")

      #Create a vector to hold a data frame with the intensity mapping values
      #in the second column and a vector holding the sorted bin labels for the map
      mapPointIntensity = mapIntensityBins(filtered$resultUserCopyNum)

      #Plotting the data points
      leaflet::leafletProxy("mymap", data = filtered) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearPopups() %>%
        #Adding labels to markers on map
        leaflet::addCircleMarkers(lng = ~as.numeric(stationDecimalLongitude),
                                  lat = ~as.numeric(stationDecimalLatitude),
                                  color = ~suppressWarnings(leaflet::colorFactor(
                                    palette = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
                                    domain = base::c(1, 2, 3, 4, 5))(mapPointIntensity$binned_data$binned)),
                                  options = leaflet::leafletOptions(zoomDelta = 0), # Disable zooming
                                  clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                                  popup= suppressWarnings(paste("<strong><h5>", filtered$assayGenus," ", filtered$assaySpecies, "</strong>",
                                                                "<br><h6>Assay|Marker:", filtered$assayName, "|",filtered$assayGeneTarget,
                                                                "<br><h6>Project|Site|Station|Replicate:", filtered$projectID,"|",filtered$siteID,"|",filtered$stationID,"|",filtered$replicateID,
                                                                "<br><h6>Extract|Assay|Run|Reaction:", as.character(filtered$extractID),"|",as.character(filtered$assayID),"|",as.character(filtered$resultRunID),"|",as.character(filtered$resultReactID),
                                                                "<br><h6>Collection Date:",filtered$replicateCollectionDate,
                                                                "<strong><h5>Thres.|Cq|Copy|Eff.:", as.numeric(sprintf("%.2f", as.numeric(filtered$resultUserProvThres))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$resultUserProvCq))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$userCopyNum))),"|",as.numeric(sprintf("%.2f",as.numeric(filtered$userPCREfficiency))),"</strong>",
                                                                "<br><h6>Type|Std Curve:", filtered$resultSampleType,"|",filtered$resultStdCurveID,
                                                                "<h6>Lab|Collector|Analyst|Platform:", filtered$projectInstitutionID,"|",filtered$replicateCollectorName,"|",filtered$extractAnalystName, "|", filtered$resultPlatform,
                                                                "<h6>Coord(Lat, Lon):", filtered$stationDecimalLatitude,",", filtered$stationDecimalLongitude))) %>%
        leaflet::addLegend(
          position = "topright",
          colors = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
          labels = mapPointIntensity$sorted_range_labels,
          title = 'resultUserCopyNum Intensity',
          opacity = 0.6
        )#end of legend


    }else if(mappedValueVal == "resultUserProvThres"){ #End of the type of points plotted

print("In resultUserProvThres")

      mapPointIntensity = mapIntensityBins(filtered$resultUserProvThres)

      #Plotting the data points
      leaflet::leafletProxy("mymap", data = filtered) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearPopups() %>%
        #Adding labels to markers on map
        leaflet::addCircleMarkers(lng = ~as.numeric(stationDecimalLongitude),
                                  lat = ~as.numeric(stationDecimalLatitude),
                                  color = ~suppressWarnings(leaflet::colorFactor(
                                    palette = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
                                    domain = base::c(1, 2, 3, 4, 5))(mapPointIntensity$binned_data$binned)),
                                  options = leaflet::leafletOptions(zoomDelta = 0), # Disable zooming
                                  clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                                  popup= suppressWarnings(paste("<strong><h5>", filtered$assayGenus," ", filtered$assaySpecies, "</strong>",
                                                                "<br><h6>Assay|Marker:", filtered$assayName, "|",filtered$assayGeneTarget,
                                                                "<br><h6>Project|Site|Station|Replicate:", filtered$projectID,"|",filtered$siteID,"|",filtered$stationID,"|",filtered$replicateID,
                                                                "<br><h6>Extract|Assay|Run|Reaction:", as.character(filtered$extractID),"|",as.character(filtered$assayID),"|",as.character(filtered$resultRunID),"|",as.character(filtered$resultReactID),
                                                                "<br><h6>Collection Date:",filtered$replicateCollectionDate,
                                                                "<strong><h5>Thres.|Cq|Copy|Eff.:", as.numeric(sprintf("%.2f", as.numeric(filtered$resultUserProvThres))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$resultUserProvCq))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$userCopyNum))),"|",as.numeric(sprintf("%.2f",as.numeric(filtered$userPCREfficiency))),"</strong>",
                                                                "<br><h6>Type|Std Curve:", filtered$resultSampleType,"|",filtered$resultStdCurveID,
                                                                "<h6>Lab|Collector|Analyst|Platform:", filtered$projectInstitutionID,"|",filtered$replicateCollectorName,"|",filtered$extractAnalystName, "|", filtered$resultPlatform,
                                                                "<h6>Coord(Lat, Lon):", filtered$stationDecimalLatitude,",", filtered$stationDecimalLongitude))) %>%
        leaflet::addLegend(
          position = "topright",
          colors = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
          labels = mapPointIntensity$sorted_range_labels,
          title = 'resultUserProvThres Intensity',
          opacity = 0.6
        )#end of legend

    } else if(mappedValueVal == "mdmaprCq"){

print("In mdmaprCq")

      #Create a vector to hold a data frame with the intensity mapping values
      #in the second column and a vector holding the sorted bin labels for the map
      mapPointIntensity = mapIntensityBins(filtered$mdmaprCq)

      #Plotting the data points
      leaflet::leafletProxy("mymap", data = filtered) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearPopups() %>%
        #Adding labels to markers on map
        leaflet::addCircleMarkers(lng = ~as.numeric(stationDecimalLongitude),
                                  lat = ~as.numeric(stationDecimalLatitude),
                                  color = ~suppressWarnings(leaflet::colorFactor(
                                    palette = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
                                    domain = base::c(1, 2, 3, 4, 5))(mapPointIntensity$binned_data$binned)),
                                  options = leaflet::leafletOptions(zoomDelta = 0), # Disable zooming
                                  clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                                  popup= suppressWarnings(paste("<strong><h5>", filtered$assayGenus," ", filtered$assaySpecies, "</strong>",
                                                                "<br><h6>Assay|Marker:", filtered$assayName, "|",filtered$assayGeneTarget,
                                                                "<br><h6>Project|Site|Station|Replicate:", filtered$projectID,"|",filtered$siteID,"|",filtered$stationID,"|",filtered$replicateID,
                                                                "<br><h6>Extract|Assay|Run|Reaction:", as.character(filtered$extractID),"|",as.character(filtered$assayID),"|",as.character(filtered$resultRunID),"|",as.character(filtered$resultReactID),
                                                                "<br><h6>Collection Date:",filtered$replicateCollectionDate,
                                                                "<strong><h5>Thres.|Cq|Copy|Eff.:", as.numeric(sprintf("%.2f", as.numeric(filtered$mdmaprThres))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprCq))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprCopyNum))),"|",as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprPCREfficiency))),"</strong>",
                                                                "<br><h6>Type|Std Curve:", filtered$resultSampleType,"|",filtered$resultStdCurveID,
                                                                "<h6>Lab|Collector|Analyst|Platform:", filtered$projectInstitutionID,"|",filtered$replicateCollectorName,"|",filtered$extractAnalystName, "|", filtered$resultPlatform,
                                                                "<h6>Coord(Lat, Lon):", filtered$stationDecimalLatitude,",", filtered$stationDecimalLongitude))) %>%
        leaflet::addLegend(
          position = "topright",
          colors = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
          labels = mapPointIntensity$sorted_range_labels,
          title = 'mdmaprCq Intensity',
          opacity = 0.6
        )#end of legend

    } else if(mappedValueVal == "mdmaprCopyNum"){

print("In mdmaprCopyNum")

      #Create a vector to hold a data frame with the intensity mapping values
      #in the second column and a vector holding the sorted bin labels for the map
      mapPointIntensity = mapIntensityBins(filtered$mdmaprCopyNum)

      #Plotting the data points
      leaflet::leafletProxy("mymap", data = filtered) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearPopups() %>%
        #Adding labels to markers on map
        leaflet::addCircleMarkers(lng = ~as.numeric(stationDecimalLongitude),
                                  lat = ~as.numeric(stationDecimalLatitude),
                                  color = ~suppressWarnings(leaflet::colorFactor(
                                    palette = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
                                    domain = base::c(1, 2, 3, 4, 5))(mapPointIntensity$binned_data$binned)),
                                  options = leaflet::leafletOptions(zoomDelta = 0), # Disable zooming
                                  clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                                  popup= suppressWarnings(paste("<strong><h5>", filtered$assayGenus," ", filtered$assaySpecies, "</strong>",
                                                                "<br><h6>Assay|Marker:", filtered$assayName, "|",filtered$assayGeneTarget,
                                                                "<br><h6>Project|Site|Station|Replicate:", filtered$projectID,"|",filtered$siteID,"|",filtered$stationID,"|",filtered$replicateID,
                                                                "<br><h6>Extract|Assay|Run|Reaction:", as.character(filtered$extractID),"|",as.character(filtered$assayID),"|",as.character(filtered$resultRunID),"|",as.character(filtered$resultReactID),
                                                                "<br><h6>Collection Date:",filtered$replicateCollectionDate,
                                                                "<strong><h5>Thres.|Cq|Copy|Eff.:", as.numeric(sprintf("%.2f", as.numeric(filtered$mdmaprThres))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprCq))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprCopyNum))),"|",as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprPCREfficiency))),"</strong>",
                                                                "<br><h6>Type|Std Curve:", filtered$resultSampleType,"|",filtered$resultStdCurveID,
                                                                "<h6>Lab|Collector|Analyst|Platform:", filtered$projectInstitutionID,"|",filtered$replicateCollectorName,"|",filtered$extractAnalystName, "|", filtered$resultPlatform,
                                                                "<h6>Coord(Lat, Lon):", filtered$stationDecimalLatitude,",", filtered$stationDecimalLongitude))) %>%
        leaflet::addLegend(
          position = "topright",
          colors = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
          labels = mapPointIntensity$sorted_range_labels,
          title = 'mdmaprCopyNum Intensity',
          opacity = 0.6
        )#end of legend

    } else if(mappedValueVal == "mdmaprThres"){

print("In mdmaprThres")

      #Create a vector to hold a data frame with the intensity mapping values
      #in the second column and a vector holding the sorted bin labels for the map
      mapPointIntensity = mapIntensityBins(filtered$mdmaprThres)

      #Plotting the data points
      leaflet::leafletProxy("mymap", data = filtered) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearPopups() %>%
        #Adding labels to markers on map
        leaflet::addCircleMarkers(lng = ~as.numeric(stationDecimalLongitude),
                                  lat = ~as.numeric(stationDecimalLatitude),
                                  color = ~suppressWarnings(leaflet::colorFactor(
                                    palette = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
                                    domain = base::c(1, 2, 3, 4, 5))(mapPointIntensity$binned_data$binned)),
                                  options = leaflet::leafletOptions(zoomDelta = 0), # Disable zooming
                                  clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                                  popup= suppressWarnings(paste("<strong><h5>", filtered$assayGenus," ", filtered$assaySpecies, "</strong>",
                                                                "<br><h6>Assay|Marker:", filtered$assayName, "|",filtered$assayGeneTarget,
                                                                "<br><h6>Project|Site|Station|Replicate:", filtered$projectID,"|",filtered$siteID,"|",filtered$stationID,"|",filtered$replicateID,
                                                                "<br><h6>Extract|Assay|Run|Reaction:", as.character(filtered$extractID),"|",as.character(filtered$assayID),"|",as.character(filtered$resultRunID),"|",as.character(filtered$resultReactID),
                                                                "<br><h6>Collection Date:",filtered$replicateCollectionDate,
                                                                "<strong><h5>Thres.|Cq|Copy|Eff.:", as.numeric(sprintf("%.2f", as.numeric(filtered$mdmaprThres))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprCq))), "|", as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprCopyNum))),"|",as.numeric(sprintf("%.2f",as.numeric(filtered$mdmaprPCREfficiency))),"</strong>",
                                                                "<br><h6>Type|Std Curve:", filtered$resultSampleType,"|",filtered$resultStdCurveID,
                                                                "<h6>Lab|Collector|Analyst|Platform:", filtered$projectInstitutionID,"|",filtered$replicateCollectorName,"|",filtered$extractAnalystName, "|", filtered$resultPlatform,
                                                                "<h6>Coord(Lat, Lon):", filtered$stationDecimalLatitude,",", filtered$stationDecimalLongitude))) %>%
        leaflet::addLegend(
          position = "topright",
          colors = base::c("#fbd300","#ff8600","#ea5f94","#9d02d7","#0000ff"),
          labels = mapPointIntensity$sorted_range_labels,
          title = 'mdmaprThres Intensity',
          opacity = 0.6
        )#end of legend
    }#End of if else populating the mapping points
  }#End of mapping data points function

  #Update the filtering page
  ######################################################################
  filterOptionsUpdate <- function() {

print("filterOptionsUpdate - Begin ")
    #First get all of the selected values for the filters
    projectIDVal <- input$projectID_input
    machineVal <- input$machine_input
    targetGeneVal <- input$targetGene_input
    mappedValueVal$value <- input$mappedValueButton
    mappedValIncludeEmptyVal <- input$mappedValIncludeEmpty
    assayVal <- input$assay_input
    sampleTypeVal <- input$resultSampleType_input
    earlyDateVal <- input$date_input[1]
    lateDateVal <- input$date_input[2]
    mappedDateIncludeEmptyVal <- input$mappedDateIncludeEmpty
    genusVal <- input$genus_input
    speciesVal <- input$species_input

    #Then update the filtered$value using the totalMDMAPRDataFile$value table
    filtered$value <- totalMDMAPRDataFile$value[totalMDMAPRDataFile$value$projectID %in% projectIDVal,,drop=FALSE]
    filtered$value <- filtered$value[filtered$value$resultPlatform %in% machineVal,,drop=FALSE]
    filtered$value <- filtered$value[filtered$value$assayGeneTarget %in% targetGeneVal,,drop=FALSE]


    if(!mappedValIncludeEmptyVal){
      # Filter dataframe to keep rows where the user-specified mapping data point column is not "Unable to compute" OR NA
      filtered$value <- filtered$value[filtered$value[[mappedValueVal$value]] != "Unable to compute", ,drop=FALSE]
    }
    filtered$value <- filtered$value[!is.na(filtered$value[[mappedValueVal$value]]), ,drop=FALSE]

    #Update filtered$value by assay name and sample type
    filtered$value <- filtered$value[filtered$value$assayName %in% assayVal,,drop=FALSE]
    filtered$value <- filtered$value[filtered$value$resultSampleType %in% sampleTypeVal,,drop=FALSE]

    #Update and only keep rows between the dates.
    #    filtered$value <- filtered$value[filtered$value$replicateCollectionDate >= earlyDateVal & filtered$value$replicateCollectionDate <= lateDateVal, , drop=FALSE]

    if(mappedDateIncludeEmptyVal){
      #Including NA and blanks
      filtered$value <- filtered$value[
        is.na(filtered$value$replicateCollectionDate) |
          filtered$value$replicateCollectionDate == "" |
          (filtered$value$replicateCollectionDate >= earlyDateVal &
             filtered$value$replicateCollectionDate <= lateDateVal), ,drop=FALSE]
    }else {

      filtered$value <- filtered$value[filtered$value$replicateCollectionDate >= earlyDateVal &
                                         filtered$value$replicateCollectionDate <= lateDateVal, ,drop=FALSE]

    }

    #Update filtered$value by genus and species
    filtered$value <- filtered$value[filtered$value$assayGenus %in% genusVal,,drop=FALSE]
    filtered$value <- filtered$value[filtered$value$assaySpecies %in% speciesVal,,drop=FALSE]

    if(nrow(filtered$value)>0){
      #Now take the values selected and the values remaining and only keep the overlapping
      projectIDVal <- intersect(projectIDVal, unique(totalMDMAPRDataFile$value$projectID))
      machineVal <- intersect(machineVal, unique(totalMDMAPRDataFile$value$resultPlatform))
      targetGeneVal <- intersect(targetGeneVal, unique(totalMDMAPRDataFile$value$assayGeneTarget))
      assayVal <- intersect(assayVal, unique(totalMDMAPRDataFile$value$assayName))
      sampleTypeVal <- intersect(sampleTypeVal, unique(totalMDMAPRDataFile$value$resultSampleType))
      genusVal <- intersect(genusVal, unique(totalMDMAPRDataFile$value$assayGenus))
      speciesVal <- intersect(speciesVal, unique(totalMDMAPRDataFile$value$assaySpecies))

      #Update different filters using the main dataframe but using the post filtered values
      shinyWidgets::updatePickerInput(session, "projectID_input", choices = sort(unique(totalMDMAPRDataFile$value$projectID), na.last = TRUE), selected = projectIDVal)
      shinyWidgets::updatePickerInput(session, "machine_input", choices = sort(unique(totalMDMAPRDataFile$value$resultPlatform), na.last = TRUE), selected = machineVal)
      shinyWidgets::updatePickerInput(session, "targetGene_input", choices = sort(unique(totalMDMAPRDataFile$value$assayGeneTarget), na.last = TRUE), selected = targetGeneVal)
      shiny::updateRadioButtons(session, "mappedValueButton", selected = mappedValueVal$value)
      shiny::updateCheckboxInput(session, "mappedValIncludeEmpty", value = mappedValIncludeEmptyVal)
      shinyWidgets::updatePickerInput(session, "assay_input", choices = sort(unique(totalMDMAPRDataFile$value$assayName), na.last = TRUE), selected = assayVal)
      shinyWidgets::updatePickerInput(session, "resultSampleType_input", choices = sort(unique(totalMDMAPRDataFile$value$resultSampleType), na.last = TRUE), selected = sampleTypeVal)

      # Get all of the date entries of interest
      cleaned_dates <- totalMDMAPRDataFile$value$replicateCollectionDate
      # Remove empty strings and NA values
      cleaned_dates <- cleaned_dates[cleaned_dates != "" & !is.na(cleaned_dates)]
      # Convert to Date class
      cleaned_dates <- as.Date(cleaned_dates)

      if(min(cleaned_dates)<=earlyDateVal){
        startDate<-min(cleaned_dates)
      }else{
        startDate<-earlyDateVal
      }
      if(max(cleaned_dates)>lateDateVal){
        endDate<-max(cleaned_dates)
      }else{
        endDate<-lateDateVal
      }
      shiny::updateSliderInput(session = session, inputId = "date_input", min = startDate, max = endDate, value=base::c(as.Date(earlyDateVal,"%Y-%m-%d"),as.Date(lateDateVal,"%Y-%m-%d")),step = 1)
      shiny::updateCheckboxInput(session, "mappedDateIncludeEmpty", value = mappedDateIncludeEmptyVal)
      shinyWidgets::updatePickerInput(session, "genus_input", choices = sort(unique(totalMDMAPRDataFile$value$assayGenus), na.last = TRUE), selected = genusVal)
      shinyWidgets::updatePickerInput(session, "species_input", choices = sort(unique(totalMDMAPRDataFile$value$assaySpecies), na.last = TRUE), selected = speciesVal)

    }else{

      shiny::showModal(shiny::modalDialog(
        title = "Data Filtering",
        "No data remain after filtering. Please try again."
      ))

    }

print("filterOptionsUpdate - End ")

  }

  ############## Data Processing Modals used Upon Hitting Submit ###############
  # A modal dialog checking to see there is data present in the totalMDMAPRDataFile$value
  dataPresentCheckModalMetaOnly <- function(failed = FALSE) {
    print(paste("In the dataPresentCheckModal at time - ", format(Sys.time(), "%Y_%m_%d_%H%M%S")))
    shiny::modalDialog(
      shiny::p('Please select if you would like to overwrite the data or combine the data...'),
      if (failed)
        div(shiny::tags$b("Invalid selection please try again!", style = "color: red;")),
      footer = shiny::tagList(
        shiny::actionButton("overwrite_dataMetaOnly", "Overwrite"),
        shiny::actionButton("combo_dataMetaOnly", "Combine")
      )
    )
  }

  # A modal dialog checking to see there is data present in the totalMDMAPRDataFile$value
  dataPresentCheckModalBothData <- function(failed = FALSE) {
    print(paste("In the dataPresentCheckModal at time - ", format(Sys.time(), "%Y_%m_%d_%H%M%S")))
    shiny::modalDialog(
      shiny::p('Please select if you would like to overwrite the data or combine the data...'),
      if (failed)
        div(shiny::tags$b("Invalid selection please try again!", style = "color: red;")),
      footer = shiny::tagList(
        shiny::actionButton("overwrite_dataBothData", "Overwrite"),
        shiny::actionButton("combo_dataBothData", "Combine")
      )
    )
  }

  #Checking the data and asking if it should be recalculated should be
  askCalcCheckModal <- function(failed = FALSE) {
    shiny::modalDialog(
      shiny::p("Would you like to generate calculated values for these data and then visualize or just visualize? Please note that only records with complete visualization data (including absorbance data) will be used."),
      if (failed)
        div(shiny::tags$b("Invalid selection please try again!", style = "color: red;")),
      footer = shiny::tagList(
        shiny::actionButton("calc_and_visualize", "Calculate and Visualize"),
        shiny::actionButton("visualize", "Visualize")
      )
    )
  }

  ####################### Reset the filtering page #############################
  resetFilterMapping <- function() {
    #Update different filters using the main dataframe
    shinyWidgets::updatePickerInput(session, "projectID_input", choices = sort(unique(totalMDMAPRDataFile$value$projectID), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$projectID))
    shinyWidgets::updatePickerInput(session, "machine_input", choices = sort(unique(totalMDMAPRDataFile$value$resultPlatform), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$resultPlatform))
    shinyWidgets::updatePickerInput(session, "targetGene_input", choices = sort(unique(totalMDMAPRDataFile$value$assayGeneTarget), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assayGeneTarget))

    shiny::updateRadioButtons(session, "mappedValueButton", selected = "resultUserProvCq")
    shiny::updateCheckboxInput(session, "mappedValIncludeEmpty", value = TRUE)

    shinyWidgets::updatePickerInput(session, "assay_input", choices = sort(unique(totalMDMAPRDataFile$value$assayName), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assayName))
    shinyWidgets::updatePickerInput(session, "resultSampleType_input", choices = sort(unique(totalMDMAPRDataFile$value$resultSampleType), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$resultSampleType))

    # Get all of the date entries of interest
    cleaned_dates <- totalMDMAPRDataFile$value$replicateCollectionDate
    # Remove empty strings and NA values
    cleaned_dates <- cleaned_dates[cleaned_dates != "" & !is.na(cleaned_dates)]
    # Convert to Date class
    cleaned_dates <- as.Date(cleaned_dates)

    shiny::updateSliderInput(session = session, inputId = "date_input", min = min(cleaned_dates), max = max(cleaned_dates), value=base::c(min(cleaned_dates),max(cleaned_dates)),step = 1)

    shiny::updateCheckboxInput(session, "mappedDateIncludeEmpty", value = TRUE)
    shinyWidgets::updatePickerInput(session, "genus_input", choices = sort(unique(totalMDMAPRDataFile$value$assayGenus), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assayGenus))
    shinyWidgets::updatePickerInput(session, "species_input", choices = sort(unique(totalMDMAPRDataFile$value$assaySpecies), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assaySpecies))

    filtered$value <- totalMDMAPRDataFile$value

  }

  ############## Initialize variables #########################################

  #dataImport file location reactive values
  qpcr_file <- shiny::reactiveValues(data = NA)
  metadata_file <- shiny::reactiveValues(data = NA)

  #Reactive values to hold the read in metadata
  readInMetadata <- shiny::reactiveValues(value = setResetMdmaprDataFile())

  #qPCR data read in to the program
  readInqPCRData <- shiny::reactiveValues(value = data.frame(built_unique_ID = character(0), Cycle1 = numeric(0)))

  #Variable to hold the total data set for this instance of the MDMAPR
  totalMDMAPRDataFile <- shiny::reactiveValues(value = setResetMdmaprDataFile())
  output$data_present <- shiny::renderText({ as.character("No Data Loaded")})

  # Get the path where all of the folders containing the fastq files are located
  volumes = shinyFiles::getVolumes()

  #Set the filtered data set to be empty
  filtered <- shiny::reactiveValues(value = setResetMdmaprDataFile())

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "qpcr_file_button", roots = volumes, session = session)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "metadata_file_button", roots = volumes, session = session)

  #This is a mapping selection variable to use different Cq or copy numbers on the mapping
  # Initally set to the mdmaprCq
  mappedValueVal <-shiny::reactiveValues(value = "mdmaprCq")

  #Initialize the contents of the map
  setResetMap()

  #qPCR Data Overview data as reactive for filtering
  filteredqPCRDataOverview <- shiny::reactiveValues(value = setResetMdmaprDataFile())

  #Set Standard Curve Analysis datasheet so that section is ready
  filteredqStdCurveAnalysis <- shiny::reactiveValues(value = setResetMdmaprDataFile())

  ####################### Data reset button ####################################
  observeEvent(input$resetDataImport, {

print("resetDataImport - Begin")

    #Text view on data Import page
    output$qpcr_file_out <- shiny::renderText({as.character("No data selected")})
    output$metadata_file_out <- shiny::renderText({as.character("No data selected")})

    #Variables holding the path to the loaded data files
    qpcr_file$data <- NA
    metadata_file$data <- NA

    #variable to hold the submission data read into the program
    readInMetadata$value <- setResetMdmaprDataFile()
    readInqPCRData$value <- data.frame(built_unique_ID = character(0), Cycle1 = numeric(0))

    #Variable to hold the total data set for this instance of the MDMAPR
    totalMDMAPRDataFile$value <- setResetMdmaprDataFile()
    output$data_present <- shiny::renderText({ as.character("No Loaded Data" )})

    #Clear all data on the map
    setResetMap()

    #Clear the filtered data in the system
    filtered$value <- setResetMdmaprDataFile()
    mappedValueVal$value <- NULL

    #Update different filters using the main dataframe but using the post filtered values
    shinyWidgets::updatePickerInput(session, "projectID_input", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "machine_input", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "targetGene_input", choices = NULL, selected = NULL)
    shiny::updateRadioButtons(session, "mappedValueButton", selected = mappedValueVal$value)
    shiny::updateCheckboxInput(session, "mappedValIncludeEmpty", value = TRUE)
    shinyWidgets::updatePickerInput(session, "assay_input", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "resultSampleType_input", choices = NULL, selected = NULL)
    shiny::updateSliderInput(session, "date_input",
                      min = as.Date("1900-01-01"),
                      max = as.Date(Sys.Date()),
                      value = range(base::c(as.Date("1900-01-01"),
                                      as.Date(Sys.Date()))))
    shiny::updateCheckboxInput(session, "mappedDateIncludeEmpty", value = TRUE)
    shinyWidgets::updatePickerInput(session, "genus_input", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "species_input", choices = NULL, selected = NULL)

    shiny::showModal(shiny::modalDialog(
      title = "Cleared Data",
      "Data have been cleared from this MDMAPR instance!"
    ))

print("resetDataImport - End")

  },ignoreInit = TRUE)

  ###################### Data Import/Export Buttons and Pop up messages ##########
  # Show modal when qPCR button is clicked.
  shiny::observeEvent(input$qpcr_file_button, {
    tryCatch(
      expr = {
        qpcr_file_button <- shinyFiles::parseFilePaths(volumes, input$qpcr_file_button)
        qpcr_file_buttonDisplayString <- as.character(qpcr_file_button$datapath)
        qpcr_file$data<-qpcr_file_buttonDisplayString
        output$qpcr_file_out <- shiny::renderText({as.character(qpcr_file_buttonDisplayString)})
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

  # Show modal when button is clicked.
  observeEvent(input$metadata_file_button, {
    tryCatch(
      expr = {
        metadata_file_button <- shinyFiles::parseFilePaths(volumes, input$metadata_file_button)
        metadata_file_buttonDisplayString <- as.character(metadata_file_button$datapath)
        metadata_file$data<-metadata_file_buttonDisplayString
        output$metadata_file_out <- shiny::renderText({as.character(metadata_file_buttonDisplayString)})
      },
      error = function(e){
        print("Error - metadata_file_button choose file cancelled")
        metadata_file$data <- NA
      },
      warning = function(w){
        print("Warning - metadata_file_button choose file cancelled")
        metadata_file$data <- NA
      },
      finally = {
      }
    )
  },ignoreInit = TRUE)

  ####################### Data Export Download Button ##########################
  #Download button for downloading CSV of filtered mapping data
  observeEvent(input$downloadMDMAPRTable,{
    shiny::showModal(shiny::modalDialog(
      title = "Saving File",
      paste0("In directory: ", getwd(), " in file: ", format(Sys.time(), "%Y_%m_%d_%H%M%S"), "_MDMAPR_Data.tsv")
    ))
    #Remove the built_unique_ID before printing to file
    totalMDMAPRDataFile$value <- totalMDMAPRDataFile$value[ , !(names(totalMDMAPRDataFile$value) %in% "built_unique_ID")]

    #Print to file
    utils::write.table(as.data.frame(totalMDMAPRDataFile$value), file = paste0(format(Sys.time(), "%Y_%m_%d_%H%M%S"), "_MDMAPR_Data.tsv"),
                row.names = FALSE, col.names=TRUE, append = FALSE, sep="\t", quote = FALSE, na = "NA")
  },ignoreInit = TRUE)

  ####################### Data RDML table Download Button ######################
  #Download button for downloading CSV of filtered mapping data
  observeEvent(input$downloadRDMLTable,{
    shiny::showModal(shiny::modalDialog(
      title = "Saving RDML Table File",
      paste0("In directory: ", getwd(), " in file: ", format(Sys.time(), "%Y_%m_%d_%H%M%S"), "_RDML_Data.tsv")
    ))

    #Load in the RDML data
    raw_data <- RDML$new(filename = qpcr_file$data)

    # Extract fluorescence data in wide format and the metadata in a long format
    fdata <- as.data.frame(t(raw_data$GetFData(long.table = FALSE)))
    metadata <- as.data.frame(raw_data$GetFData(long.table = TRUE))

    #Work the metadata
    metadata<-metadata[,base::c(1:(ncol(metadata)-2)),drop = FALSE]
    metadata<-unique(metadata)
    metadata<-metadata[,-1,drop=FALSE]

    #Remove the row names for the fdata
    fdata<-fdata[-1,,drop=FALSE]

    # Add metadata to fluorescence data
    combined_data <- cbind(metadata, fdata)

    #Print to file
    utils::write.table(as.data.frame(combined_data), file = paste0(format(Sys.time(), "%Y_%m_%d_%H%M%S"), "_RDML_Data.tsv"),
                row.names = FALSE, col.names=TRUE, append = FALSE, sep="\t", quote = FALSE, na = "NA")

    #Reset the RDML data input
    readInqPCRData$value <- data.frame(built_unique_ID = character(0), Cycle1 = numeric(0))
    output$qpcr_file_out <- shiny::renderText({as.character()})
    output$data_present <- shiny::renderText({ as.character("No Data Loaded")})

  },ignoreInit = TRUE)

  ########## Try Upload again from RDML table Download Button Choice##############
  #Download button for downloading CSV of filtered mapping data
  observeEvent(input$uploadAgain,{
    #Reset the RDML data input
    readInqPCRData$value <- data.frame(built_unique_ID = character(0), Cycle1 = numeric(0))
    output$qpcr_file_out <- shiny::renderText({as.character()})
    output$data_present <- shiny::renderText({ as.character("No Data Loaded")})

    shiny::removeModal()

  },ignoreInit = TRUE)

  ################### Overwrite Metadata Observe Event ##################################
  #Checks the response when a set of data is submitted where
  #one is already loaded. This indicates the overwrite response
  shiny::observeEvent(input$overwrite_dataMetaOnly, {

print("overwrite_dataMetaOnly - Begin")

    #Clear all data from the totalMDMAPRDataFile
    totalMDMAPRDataFile$value <- setResetMdmaprDataFile()

    shiny::removeModal()

    #Ask if calculations should be completed
    askCalcCheckModal()

print("overwrite_dataMetaOnly - End")

  },ignoreInit = TRUE)

  ################### Overwrite all data Observe Event #########################
  #This observeEvent checks the response when a set of data is submitted where
  #one is already loaded. This indicates the overwrite response
  shiny::observeEvent(input$overwrite_dataBothData, {
    print("In the observeEvent overwrite_dataBothData before the use of setResetMdmaprDataFile - 1")
    #Clear all data from the totalMDMAPRDataFile
    totalMDMAPRDataFile$value <- setResetMdmaprDataFile()

    #Combine the read in readInMetadata$value and readInqPCRData$value
    totalMDMAPRDataFile$value <- combineReadInDataFiles(totalMDMAPRDataFile$value, readInMetadata$value, readInqPCRData$value)

    shiny::removeModal()

    #Ask if calculations should be completed
    askCalcCheckModal()
  },ignoreInit = TRUE)

  ################### Combine data Observe Event ###############################
  #This observeEvent checks the response when a set of data is submitted where
  #one is already loaded. This indicates the combine response
  shiny::observeEvent(input$combo_dataMetaOnly, {

    shiny::removeModal()

    dupRecords <- intersect(readInMetadata$value[,1], totalMDMAPRDataFile$value[,1])

    #Check if there are identical entries
    if(length(dupRecords)>0){
      shiny::showModal(shiny::modalDialog(
        title = "Duplicate data",
        "There was duplicate data records detected. The data present in the currently loaded data was overwritten with the newly submitted data and unique data was appended to the data set."
      ))
    }
      #Remove the overlapping unique data values in the stored vs the submitted file.
      totalMDMAPRDataFile$value <- totalMDMAPRDataFile$value[!totalMDMAPRDataFile$value[, 1] %in% readInMetadata$value[, 1], ]

      #Set the current totalMDMAPRDataFile to a temp file
      tempMDMAPRDataFile <- totalMDMAPRDataFile$value

      #Combine the temp and the newly loaded data sets
      totalMDMAPRDataFile$value <- plyr::rbind.fill(tempMDMAPRDataFile, totalMDMAPRDataFile$value)

      #Ask if calculations should be completed
      askCalcCheckModal()

  },ignoreInit = TRUE)
  ############### Combine when both data present observe Event #################
  #This observeEvent checks the response when a set of data is submitted where
  #one is already loaded. This indicates the combine response
  shiny::observeEvent(input$combo_dataBothData, {

    shiny::removeModal()

    dupRecords <- intersect(readInMetadata$value[,1], totalMDMAPRDataFile$value[,1])

    #Check if there are identical entries
    if(length(dupRecords)>0){
      shiny::showModal(shiny::modalDialog(
        title = "Duplicate data",
        "There was duplicate data records detected. The data present in the currently loaded data was overwritten with the newly submitted data and unique data was appended to the data set."
      ))
    }
    #Remove the overlapping unique data values in the stored vs the submitted file.
    totalMDMAPRDataFile$value <- totalMDMAPRDataFile$value[!totalMDMAPRDataFile$value[, 1] %in% readInMetadata$value[, 1], ]

    #Set the current totalMDMAPRDataFile to a temp file
    tempMDMAPRDataFile <- totalMDMAPRDataFile$value

    #Combine the read in readInMetadata$value and readInqPCRData$value
    totalMDMAPRDataFile$value <- combineReadInDataFiles(totalMDMAPRDataFile$value, readInMetadata$value, readInqPCRData$value)

    #Combine the temp and the newly loaded data sets
    totalMDMAPRDataFile$value <- plyr::rbind.fill(tempMDMAPRDataFile, totalMDMAPRDataFile$value)

    #Ask if calculations should be completed
    askCalcCheckModal()

  },ignoreInit = TRUE)
  ############### Complete calc and visualize observe Event ####################
  #This observeEvent checks the response when asked if the data should be used to complete calculations
  shiny::observeEvent(input$calc_and_visualize, {
    complete_calc()
    shiny::removeModal()
  },ignoreInit = TRUE)

  ############### Just visualize observe Event #################################
  #This observeEvent checks the response when asked if the data should be used to complete calculations
  shiny::observeEvent(input$visualize, {
    shiny::removeModal()
  },ignoreInit = TRUE)

  ################### Update Filter mapping Observe Event ######################
  #This observeEvent updates the mapping table with the filter options
  shiny::observeEvent(input$updateFilterMappingButton, {

    print("In the Observe event for the update of the filtering options")

    if (nrow(totalMDMAPRDataFile$value )==0){

      shiny::showModal(shiny::modalDialog(
        title = "Update Filtering - No Data Loaded - 1",
        "There are no data loaded in this instance of MDMAPR. Or all data have been filtered out. Please check for loaded data or your filters and try again."
      ))

    }else{

      filterOptionsUpdate()

      #Now taking the updated filtered data sheet and making it equal to the
      #qPCR Overview datasheet so that section is ready
      filteredqPCRDataOverview$value <- filtered$value
      #Set Standard Curve Analysis datasheet so that section is ready
      filteredqStdCurveAnalysis$value <- filtered$value
      # Remove any unk data as this needs to be opt or ntc data for standard curves
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultSampleType != "unkn",,drop=FALSE]


    }
  },ignoreInit = TRUE)

  ######################## Filtered table download link ########################
  shiny::observeEvent(input$downloadFilteredData, {

    #Print to file
    utils::write.table(as.data.frame(filtered$value[,-1]), file = paste0(format(Sys.time(), "%Y_%m_%d_%H%M%S"), "_Filtered_MDMAPR.tsv"),
                       row.names = FALSE, col.names=TRUE, append = FALSE, sep="\t", quote = FALSE, na = "NA")
    shiny::showModal(shiny::modalDialog(
      title = "Download Complete",
      "Please see data in the same location as your initial data files."
    ))

  },ignoreInit = TRUE)

  #This observeEvent updates the mapping table with the filter options
  shiny::observeEvent(input$resetFilterMappingButton, {

    print("In the Observe event for the reset of the filter options")

    if (nrow(totalMDMAPRDataFile$value )==0){

      shiny::showModal(shiny::modalDialog(
        title = "Reset Filtering - No Data Loaded - 1",
        "There are no data loaded in this instance of MDMAPR. Please check for loaded data and try again."
      ))

    }else{

      resetFilterMapping()

    }
  },ignoreInit = TRUE)

  ################ Submit button #################################################
  observeEvent(input$submitImport, {

  ######################## No files submitted ####################################
    #The below content is checking the files submitted adhere to MDMAPR standards

print(paste("Here is ithe contents of the RDML button - ", qpcr_file$data))
print(paste("Here is ithe contents of the MDMAPR button - ", metadata_file$data))

    if(is.null(qpcr_file$data) & is.null(metadata_file$data)){
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "There are no files present, please select data files and resubmit."
      ))
    }else if(is.na(qpcr_file$data) & is.na(metadata_file$data)){
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "There are no files present, please select data files and resubmit."
      ))

  ##################### Metadata but no qPCR data ##############################
    }else if(is.na(qpcr_file$data) & !is.na(metadata_file$data)){

print("No RDML but Meta")

      #Check to make sure that the file extension is correct
      if(grepl("\\.[Tt][Ss][Vv]$", metadata_file$data)){

        #Set the working directory to the location of the Metadata file.
        setwd(dirname(as.character(metadata_file$data)))

        #Loading in the MDMAPR data file
        tryCatch(
          expr = {

            #Load in the data to the type variable
            readInMetadata$value<-utils::read.table(metadata_file$data, header=TRUE, sep="\t", dec=".")

            #Build a unique ID to mirror the metadata file unique ID
            #  fdata <- fdata[, c( "fdata.name", "exp.id", "run.id", "react.id","fluor")]
            built_unique_ID <- readInMetadata$value[, base::c("projectID","resultRunID", "resultReactID", "assayGeneTarget", "replicateID" )]

            #Remove all whitespaces
            # Apply gsub to remove all white spaces from specified columns
            built_unique_ID[,base::c(1,2,3,4,5)] <- lapply(built_unique_ID[,base::c(1,2,3,4,5)], function(col) {
              gsub("\\s", "", col)
            })

            #Format the dataframe with a unique ID in the first Column
            built_unique_ID<-paste0(built_unique_ID$projectID,"_",built_unique_ID$resultRunID,"_", readInMetadata$value$resultReactID, "_",built_unique_ID$assayGeneTarget, "_",built_unique_ID$replicateID)
            readInMetadata$value<-cbind(built_unique_ID, readInMetadata$value)

            #Comparing the submitted file headers against the expected.
            missing_columns <- setdiff(colnames(totalMDMAPRDataFile$value)[2:ncol(totalMDMAPRDataFile$value)], colnames(readInMetadata$value))

            # Check if the loaded headers start with the expected headers
            if (length(missing_columns) > 0) {

              #variable to hold the submission data read into the program
              readInMetadata$value <- setResetMdmaprDataFile()

              shiny::modalDialog(
                title = "Incorrect File Types",
                "The submitted '.tsv' file is in an incorrect format where the headers do not match the accepted file format. Please select a properly formatted '.tsv' file(s) and resubmit."
              )

            }else {

              output$data_present <- shiny::renderText({ as.character("Loaded Data" )})

              if (nrow(totalMDMAPRDataFile$value)  == 0){

                #Set the read in data to the loaded data
                totalMDMAPRDataFile$value <- readInMetadata$value

                #Asking if the calcs should be completed or not
                shiny::showModal(askCalcCheckModal())

              }else{

                #modal to combine the data
                shiny::showModal(dataPresentCheckModalMetaOnly())

              }
            }

          },
          error = function(e){
            shiny::showModal(shiny::modalDialog(
              title = "Incorrect File Type",
              "Incorrect file type, please select a properly formatted '.tsv' file and resubmit."
            ))
            metadata_file$data <- NA

            #Add in an if to see if there is data loaded into the program, if not set the output to screen indicator to no data
            if(nrow(metadata_file$data)==0){

              output$data_present <- shiny::renderText({ as.character("No Data Loaded")})

            }
          },
          warning = function(w){
            shiny::showModal(shiny::modalDialog(
              title = "Incorrect File Type",
              "Incorrect file type, please select a properly formatted '.tsv' file and resubmit."
            ))
            metadata_file$data <- NA

            #Add in an if to see if there is data loaded into the program, if not set the output to screen indicator to no data
            if(nrow(totalMDMAPRDataFile$value)==0){

              output$data_present <- shiny::renderText({ as.character("No Data Loaded")})

            }
          }
        )#Closing trycatch

      }else{#The file extension was incorrect
        shiny::showModal(shiny::modalDialog(
          title = "Incorrect File Type - 1",
          "Incorrect file type, please select a properly formatted '.tsv' file and resubmit."
        ))
      }#End of if-else right extension

############## qPCR but no metadata ############################################

    }else if(!is.na(qpcr_file$data) & is.na(metadata_file$data)){

print("RDML but not Meta")

      if(grepl("\\.[Rr][Dd][Mm][Ll]$", qpcr_file$data)){

        #Set the working directory to the location of the Metadata file.
        setwd(dirname(as.character(qpcr_file$data)))

        tryCatch(
          expr = {

            if (nrow(totalMDMAPRDataFile$value)  == 0){
              shiny::showModal(shiny::modalDialog(
                title = "No MDMAPR data present",
                "No MDMAPR data are loaded and no MDMAPR data file was submitted. You can download a table version of the RDML data or try to upload again.",
                footer = shiny::tagList(
                  shiny::actionButton("downloadRDMLTable", "Download RDML data table"),
                  shiny::actionButton("uploadAgain", "Try Upload Again")
                )
              ))
            }else{

              #Load in the RDML data
              raw_data <- RDML$new(filename = qpcr_file$data)

              #pull all the fluorescence data
              fdata <- as.data.frame(raw_data$GetFData(long.table = T))

              #Process the data using the function process_Multiplexed_RDML
              readInqPCRData$value <- process_Multiplexed_RDML(fdata)

              #Make the currently loaded data equal to to newly loaded data
              readInMetadata$value <- totalMDMAPRDataFile$value

              #Combine the read in readInMetadata$value and readInqPCRData$value
              totalMDMAPRDataFile$value <- combineReadInDataFiles(totalMDMAPRDataFile$value, readInMetadata$value, readInqPCRData$value)

              #Run the calculation function
              complete_calc()

            }
          },
          error = function(e){
            print("Error - loading the RDML data file, please check the file and try again.")
            shiny::showModal(shiny::modalDialog(
              title = "Incorrect File Type - Error",
              "Incorrect file type, please select a properly formatted '.rdml' file and resubmit."
            ))
          },
          warning = function(w){
            print("Error - loading the RDML data file, please check the file and try again.")
            shiny::showModal(shiny::modalDialog(
              title = "Incorrect File Type - Warning",
              "Incorrect file type, please select a properly formatted '.rdml' file and resubmit."
            ))
          }
        )#Closing trycatch
      }else{#The file extension was incorrect
        shiny::showModal(shiny::modalDialog(
          title = "Incorrect File Extension",
          "Incorrect file type, please select a properly formatted '.rdml' file and resubmit."
        ))
      }#End of if-else right extension

############### Both qPCR and Metadata #########################################

    }else if(!is.na(qpcr_file$data) & !is.na(metadata_file$data)){

#print("RDML and Meta - 1")

      if(grepl("\\.[Rr][Dd][Mm][Ll]$", qpcr_file$data) & grepl("\\.[Tt][Ss][Vv]$", metadata_file$data)){

#print("RDML and Meta - 2")

        #Set the working directory to the location of the Metadata file.
        setwd(dirname(as.character(metadata_file$data)))

#print("RDML and Meta - 3")

        #Loading in the MDMAPR data files
        tryCatch(
          expr = {

#print("RDML and Meta - 4")

            #Load in the data to the type variable
            readInMetadata$value<-utils::read.table(metadata_file$data, header=TRUE, sep="\t", dec=".")

#print("RDML and Meta - 5")

            #Build a unique ID to mirror the metadata file unique ID
            #  fdata <- fdata[, c( "fdata.name", "exp.id", "run.id", "react.id","fluor")]
            built_unique_ID <- readInMetadata$value[, base::c("projectID","resultRunID", "resultReactID", "assayGeneTarget", "replicateID" ), drop=FALSE]

#print("RDML and Meta - 6")
built_unique_IDGlobalRDMLMeta6<<-built_unique_ID

            #Remove all whitespaces
            # Apply gsub to remove all white spaces from specified columns
            built_unique_ID[,base::c(1,2,3,4,5)] <- lapply(built_unique_ID[,base::c(1,2,3,4,5)], function(col) {
              gsub("\\s", "", col)
            })

#print("RDML and Meta - 7")

            #Format the dataframe with a unique ID in the first Column
            built_unique_ID<-paste0(built_unique_ID$projectID,"_",built_unique_ID$resultRunID,"_", readInMetadata$value$resultReactID, "_",built_unique_ID$assayGeneTarget, "_",built_unique_ID$replicateID)
            readInMetadata$value<-cbind(built_unique_ID, readInMetadata$value)

#print("RDML and Meta - 8")

            #Get the columns to check that the file is in the correct format
            missing_columns <- setdiff(colnames(totalMDMAPRDataFile$value), colnames(readInMetadata$value))

#print("RDML and Meta - 9")

            # Check if the loaded headers start with the expected headers
            if (length(missing_columns) > 0) {

#print("RDML and Meta - 10")

              #variable to hold the submission data read into the program
              readInMetadata$value <- setResetMdmaprDataFile()

#print("RDML and Meta - 11")

              shiny::modalDialog(
                title = "Incorrect File Types",
                "The submitted '.tsv' file is in an incorrect format where the headers do not match the accepted file format. Please select a properly formatted '.tsv' file(s) and resubmit."
              )

#print("RDML and Meta - 12")

            }else {

#print("RDML and Meta - 13")

              #Load in the RDML data
              raw_data <- RDML$new(filename = qpcr_file$data)

#print("RDML and Meta - 14")

              #pull all the fluorescence data
              fdata <- as.data.frame(raw_data$GetFData(long.table = T))

#print("RDML and Meta - 15")

              #Process the data using the function process_Multiplexed_RDML
              readInqPCRData$value <- process_Multiplexed_RDML(fdata)

#print("RDML and Meta - 16")

              if (nrow(totalMDMAPRDataFile$value)  == 0){

#print("RDML and Meta - 17")

                #Combine the read in readInMetadata$value and readInqPCRData$value
                totalMDMAPRDataFile$value <- combineReadInDataFiles(totalMDMAPRDataFile$value, readInMetadata$value, readInqPCRData$value)

#print("RDML and Meta - 18")

                #Run the calculation function
                complete_calc()

#print("RDML and Meta - 19")

              }else{

#print("RDML and Meta - 20")

                shiny::showModal(dataPresentCheckModalBothData())

#print("RDML and Meta - 21")

              }#Close off the else from to check if there was data already loaded

#print("RDML and Meta - 22")

              output$data_present <- shiny::renderText({ as.character("Loaded Data" )})

#print("RDML and Meta - 23")

            }#Close off the if to check if the data format of the metadata is correct
          },#Close off the try catch
          error = function(e){
            print("Error - 1 - loading one of the data files, please check the files and try again.")
          },
          warning = function(w){
            print("Error - 2 - loading one of the data files, please check the files and try again.")
          }
        )#Closing trycatch
      }else{#The file extension was incorrect
        shiny::modalDialog(
          title = "Incorrect File Types",
          "Incorrect file type(s), please select a properly formatted '.rdml' and/or '.tsv' file(s) and resubmit."
        )
      }#End of checking file name

    }#End of checking files

#print("Data Submit - 2")

    #reset the variables to hold the submission data read into the program
    readInMetadata$value <- setResetMdmaprDataFile()
    readInqPCRData$value <- data.frame(built_unique_ID = character(0), Cycle1 = numeric(0))

#print("Data Submit - 3")

    #Set the initial filtered dataset to the uploaded data.
    filtered$value <- totalMDMAPRDataFile$value
    #Set qPCR Overview datasheet so that section is ready
    filteredqPCRDataOverview$value <- totalMDMAPRDataFile$value

    #Set Standard Curve Analysis datasheet so that section is ready
    filteredqStdCurveAnalysis$value <- totalMDMAPRDataFile$value
    # Remove any unk data as this needs to be opt or ntc data for standard curves
    filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultSampleType != "unkn",,drop=FALSE]

    #Initially set the results for the mapping to the user supplied Cq
    mappedValueVal$value <- "resultUserProvCq"

#print("Data Submit - 4")

    #Update different filters using the main dataframe
    shinyWidgets::updatePickerInput(session, "projectID_input", choices = sort(unique(totalMDMAPRDataFile$value$projectID), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$projectID))
    shinyWidgets::updatePickerInput(session, "machine_input", choices = sort(unique(totalMDMAPRDataFile$value$resultPlatform), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$resultPlatform))
    shinyWidgets::updatePickerInput(session, "targetGene_input", choices = sort(unique(totalMDMAPRDataFile$value$assayGeneTarget), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assayGeneTarget))
    shiny::updateRadioButtons(session, "mappedValueButton", selected = mappedValueVal$value)
    shiny::updateCheckboxInput(session, "mappedValIncludeEmpty", value = TRUE)
    shinyWidgets::updatePickerInput(session, "assay_input", choices = sort(unique(totalMDMAPRDataFile$value$assayName), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assayName))
    shinyWidgets::updatePickerInput(session, "resultSampleType_input", choices = sort(unique(totalMDMAPRDataFile$value$resultSampleType), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$resultSampleType))
    # Get all of the date entries of interest
    cleaned_dates <- totalMDMAPRDataFile$value$replicateCollectionDate
    # Remove empty strings and NA values
    cleaned_dates <- cleaned_dates[cleaned_dates != "" & !is.na(cleaned_dates)]
    # Convert to Date class
    cleaned_dates <- as.Date(cleaned_dates)
    shiny::updateSliderInput(session = session, inputId = "date_input", min = min(cleaned_dates), max = max(cleaned_dates), value=base::c(min(cleaned_dates),max(cleaned_dates)),step = 1)
    shiny::updateCheckboxInput(session, "mappedDateIncludeEmpty", value = TRUE)
    shinyWidgets::updatePickerInput(session, "genus_input", choices = sort(unique(totalMDMAPRDataFile$value$assayGenus), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assayGenus))
    shinyWidgets::updatePickerInput(session, "species_input", choices = sort(unique(totalMDMAPRDataFile$value$assaySpecies), na.last = TRUE), selected = unique(totalMDMAPRDataFile$value$assaySpecies))

print("End of the data submit")
totalMDMAPRDataFileGlobalEndofDataSubmit<<-totalMDMAPRDataFile$value
  },ignoreInit = TRUE)# end of the observeEvent Input Submit

######################## Side menu observe events ##############################
  shiny::observeEvent(input$sidebarMenu,{
    if(input$sidebarMenu == "mapDashboard") {
      if (nrow(filtered$value) >0){
        #Place the data points on the map
        setMappingDataPoints(filtered$value, mappedValueVal$value)
      }else{
        #Clear all data on the map
        setResetMap()
      }
    }else if(input$sidebarMenu == "qPCRDataOverviewPage"){

      print("In the qPCR data overview page")

      filteredqPCRDataOverviewGlobalqPCROverview <<-filteredqPCRDataOverview$value

      #Populate the pickers
      updatePickerInput(session, "project_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$projectID),
                        selected =  NULL)

      updatePickerInput(session, "site_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$siteID),
                        selected =  NULL)

      updatePickerInput(session, "station_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$stationID),
                        selected =  NULL)

      updatePickerInput(session, "replicate_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$replicateID),
                        selected =  NULL)

      updatePickerInput(session, "extractID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$extractID),
                        selected =  NULL)

      updatePickerInput(session, "assay_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$assayID),
                        selected =  NULL)

      updatePickerInput(session, "runID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$resultRunID),
                        selected =  NULL)

      updatePickerInput(session, "reactionID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$resultReactID),
                        selected =  NULL)


    } else if(input$sidebarMenu == "stdCurve"){

      print("In the Standard Curve page")

      #Populate the pickers
      updatePickerInput(session, "SC_project_input",
                        choices = unique(filteredqStdCurveAnalysis$value$projectID),
                        selected =  NULL)

      updatePickerInput(session, "SC_assay_input",
                        choices = unique(filteredqStdCurveAnalysis$value$assayID),
                        selected =  NULL)

      updatePickerInput(session, "SC_run_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultRunID),
                        selected =  NULL)

      updatePickerInput(session, "SC_platform_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultPlatform),
                        selected =  NULL)

      updatePickerInput(session, "SC_machine_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultMachineID),
                        selected =  NULL)

      updatePickerInput(session, "SC_curve_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultStdCurveID),
                        selected =  NULL)

      updatePickerInput(session, "SC_react_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultReactID),
                        selected =  NULL)
    }
  },ignoreInit = TRUE)

######## Mapping and Filtering Data tab observe events #########################
  shiny::observeEvent(input$mapTabs, {
    if(input$mapTabs == "Mapping") {
print("Begin observe -  input$mapTabs == Mapping")
      if (nrow(filtered$value) >0){
        print("In the mapping if there is data section - 1")
        #Place the data points on the map
        setMappingDataPoints(filtered$value, mappedValueVal$value)
        print("At the end of the mapping if there is data section - 2")
      }else{

        #Clear all data on the map
        setResetMap()

      }
    }else if(input$mapTabs == "Mapped Data Table") {
      print("Begin observe -  input$mapTabs == Mapped Data Table")
      output$mapping_data <- DT::renderDataTable({
        datatable(filtered$value, options = list(scrollX = TRUE,
                                                 autoWidth = TRUE,
                                                 columnDefs = list(list(width = '500px', targets = base::c(84)))))})
    }
  },ignoreInit = TRUE)

################## qPCR Data Overview observe events ###########################
  shiny::observeEvent(input$resetqPCRselections, {

    filteredqPCRDataOverview$value <- filtered$value
    #Populate the pickers
    updatePickerInput(session, "project_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$projectID),
                      selected =  NULL)

    updatePickerInput(session, "site_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$siteID),
                      selected =  NULL)

    updatePickerInput(session, "station_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$stationID),
                      selected =  NULL)

    updatePickerInput(session, "replicate_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$replicateID),
                      selected =  NULL)

    updatePickerInput(session, "extractID_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$extractID),
                      selected =  NULL)

    updatePickerInput(session, "assay_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$assayID),
                      selected =  NULL)

    updatePickerInput(session, "runID_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$resultRunID),
                      selected =  NULL)

    updatePickerInput(session, "reactionID_qPCROverview",
                      choices = unique(filteredqPCRDataOverview$value$resultReactID),
                      selected =  NULL)

    output$qPCROverviewPlot <- NULL

  })


  shiny::observeEvent(input$updateqPCRselections, {

    print("updateqPCRselections - Begin")

    overviewSelectedProjectID <- input$project_qPCROverview
    overviewSelectedsiteID <- input$site_qPCROverview
    overviewSelectedstationID <- input$station_qPCROverview
    overviewSelectedreplicateID <- input$replicate_qPCROverview
    overviewSelectedextractID <- input$extractID_qPCROverview
    overviewSelectedassayID <- input$assay_qPCROverview
    overviewSelectedResultRunID <- input$runID_qPCROverview
    overviewSelectedResultReactID <- input$reactionID_qPCROverview

    if(!is.null(overviewSelectedProjectID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$projectID %in% input$project_qPCROverview,,drop=FALSE]
      print(paste0("after project filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedsiteID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$siteID %in% input$site_qPCROverview,,drop=FALSE]
      print(paste0("after site filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedstationID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$stationID %in% input$station_qPCROverview,,drop=FALSE]
      print(paste0("after station filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedreplicateID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$replicateID %in% input$replicate_qPCROverview,,drop=FALSE]
      print(paste0("after replicate filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedextractID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$extractID %in% input$extractID_qPCROverview,,drop=FALSE]
      print(paste0("after extract filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedassayID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$assayID %in% input$assay_qPCROverview,,drop=FALSE]
      print(paste0("after station assay - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedResultRunID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$resultRunID %in% input$runID_qPCROverview,,drop=FALSE]
      print(paste0("after run filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(!is.null(overviewSelectedResultReactID)){
      filteredqPCRDataOverview$value <- filteredqPCRDataOverview$value[filteredqPCRDataOverview$value$resultReactID %in% input$reactionID_qPCROverview,,drop=FALSE]
      print(paste0("after reaction filter - Rows of data - ", nrow(filteredqPCRDataOverview$value)))
    }
    if(length(unique(filteredqPCRDataOverview$value$projectID))==1){
      updatePickerInput(session, "project_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$projectID),
                        selected = unique(filteredqPCRDataOverview$value$projectID))
    }else if(!is.null(overviewSelectedProjectID)){
      print("In the project")
      updatePickerInput(session, "project_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$projectID),
                        selected = overviewSelectedProjectID)
    }
    if(length(unique(filteredqPCRDataOverview$value$siteID))==1){
      updatePickerInput(session, "site_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$siteID),
                        selected = unique(filteredqPCRDataOverview$value$siteID))
    }else if(!is.null(overviewSelectedsiteID)){
      print("In the site")
      updatePickerInput(session, "site_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$siteID),
                        selected = overviewSelectedsiteID)
    }
    if(length(unique(filteredqPCRDataOverview$value$stationID))==1){
      updatePickerInput(session, "station_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$stationID),
                        selected = unique(filteredqPCRDataOverview$value$stationID))
    }else if(!is.null(overviewSelectedstationID)){
      print("In the station")
      updatePickerInput(session, "station_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$stationID),
                        selected = overviewSelectedstationID)
    }
    if(length(unique(filteredqPCRDataOverview$value$replicateID))==1){
      updatePickerInput(session, "replicate_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$replicateID),
                        selected = unique(filteredqPCRDataOverview$value$replicateID))
    }else if(!is.null(overviewSelectedreplicateID)){
      print("In the replicate")
      updatePickerInput(session, "replicate_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$replicateID),
                        selected = overviewSelectedreplicateID)
    }
    if(length(unique(filteredqPCRDataOverview$value$extractID))==1){
      updatePickerInput(session, "extractID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$extractID),
                        selected = unique(filteredqPCRDataOverview$value$extractID))
    }else if(!is.null(overviewSelectedextractID)){
      print("In the extract")
      updatePickerInput(session, "extractID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$extractID),
                        selected = overviewSelectedextractID)
    }
    if(length(unique(filteredqPCRDataOverview$value$assayID))==1){
      updatePickerInput(session, "assay_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$assayID),
                        selected = unique(filteredqPCRDataOverview$value$assayID))
    }else if(!is.null(overviewSelectedassayID)){
      print("In the assay")
      updatePickerInput(session, "assay_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$assayID),
                        selected = overviewSelectedassayID)
    }
    if(length(unique(filteredqPCRDataOverview$value$resultRunID))==1){
      updatePickerInput(session, "runID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$resultRunID),
                        selected = unique(filteredqPCRDataOverview$value$resultRunID))
    }else if(!is.null(overviewSelectedResultRunID)){
      print("In the run")
      updatePickerInput(session, "runID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$resultRunID),
                        selected = overviewSelectedResultRunID)
    }
    if(length(unique(filteredqPCRDataOverview$value$resultReactID))==1){
      updatePickerInput(session, "reactionID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$resultReactID),
                        selected = unique(filteredqPCRDataOverview$value$resultReactID))
    }else if(!is.null(overviewSelectedResultReactID)){
      print("In the reaction")
      updatePickerInput(session, "reactionID_qPCROverview",
                        choices = unique(filteredqPCRDataOverview$value$resultReactID),
                        selected = overviewSelectedResultReactID)
    }

    if(nrow(filteredqPCRDataOverview$value)==1){

      print("In the populate the figure section")

      absorbance <- filteredqPCRDataOverview$value[,grep("Cycle", names(filteredqPCRDataOverview$value), value = TRUE)]

      cycles <- 1:length(absorbance)

      # First set of thresholds
      mdmapr_Thres <- as.numeric(as.character(filteredqPCRDataOverview$value$mdmaprThres[1]))
      mdmapr_LOD <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$mdmaprStdCurveLineEq[1],filteredqPCRDataOverview$value$mdmaprStdCurveLOD95[1])))
      mdmapr_LOQ <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$mdmaprStdCurveLineEq[1],filteredqPCRDataOverview$value$mdmaprStdCurveLOQ35[1])))
      mdmapr_Cq <- as.numeric(as.character(filteredqPCRDataOverview$value$mdmaprCq[1]))
      mdmapr_LogLinear <- as.character(filteredqPCRDataOverview$value$mdmaprLogLinear[1])
      mdmapr_LogLinear <- as.numeric(unlist(strsplit(mdmapr_LogLinear, ":")))

      # Second set of thresholds
      resultUserProv_Thres <- as.numeric(as.character(filteredqPCRDataOverview$value$resultUserProvThres[1]))
      resultUserProv_LOD <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$userStdCurveLineEq[1],filteredqPCRDataOverview$value$resultUserProvLOD[1])))
      resultUserProv_LOQ <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$userStdCurveLineEq[1],filteredqPCRDataOverview$value$resultUserProvLOQ[1])))
      resultUserProv_Cq <- as.numeric(as.character(filteredqPCRDataOverview$value$resultUserProvCq[1]))
      resultUserProv_LogLinear <- as.character(filteredqPCRDataOverview$value$userLogLinear[1])
      resultUserProv_LogLinear <- as.numeric(unlist(strsplit(resultUserProv_LogLinear, ":")))

      # Determine the last non-NA value index
      last_non_na_index <- which.max(is.na(absorbance)) - 1
      if (last_non_na_index == 0) last_non_na_index <- length(absorbance)  # handle case with no NAs

      # Filter data up to the first NA
      filtered_absorbance <- as.numeric(as.character(absorbance[1:last_non_na_index]))
      filtered_cycles <- as.numeric(as.character(cycles[1:last_non_na_index]))

      # Maximum value for y-axis
      y_max <- max(filtered_absorbance, na.rm = TRUE)

      # Output plot
      output$qPCROverviewPlot <- plotly::renderPlotly({
        # Initialize plot
        p <- plotly::plot_ly()

        # Add raw absorbance data
        p <- p %>%
          plotly::add_trace(x = filtered_cycles, y = filtered_absorbance, type = 'scatter', mode = 'markers',
                            marker = list(color = 'black', size = 4),
                            name = "Raw Absorbance")

        # Add mdmapr data if checkbox is checked
        if ("mdmapr" %in% input$groups) {
          if ("thres" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(min(filtered_cycles), max(filtered_cycles)), y = rep(mdmapr_Thres, 2),
                                type = 'scatter', mode = 'lines', line = list(dash = 'dash', color = 'blue'),
                                name = 'mdmapr Threshold')
          }
          if ("lod" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(rep(mdmapr_LOD, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
                                type = 'scatter', mode = 'markers', marker = list(symbol = 'circle', color = 'blue', size = 4),
                                name = 'mdmapr LOD')
          }
          if ("loq" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(rep(mdmapr_LOQ, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
                                type = 'scatter', mode = 'markers', marker = list(symbol = 'triangle-up', color = 'blue', size = 4),
                                name = 'mdmapr LOQ')
          }
          if ("cq" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(rep(mdmapr_Cq, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
                                type = 'scatter', mode = 'markers', marker = list(symbol = 'square', color = 'blue', size = 4),
                                name = 'mdmapr Cq')
          }
          if ("loglinear" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(as.numeric(mdmapr_LogLinear[1]), as.numeric(mdmapr_LogLinear[1]), as.numeric(mdmapr_LogLinear[2]), as.numeric(mdmapr_LogLinear[2])),
                                y = c(0, y_max, y_max, 0),
                                type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(173, 216, 230, 0.2)',  # light blue
                                name = 'mdmapr Log Linear Area')
          }
        }

        # Add resultUserProv data if checkbox is checked
        if ("resultUserProv" %in% input$groups) {
          if ("thres" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(min(filtered_cycles), max(filtered_cycles)), y = rep(resultUserProv_Thres, 2),
                                type = 'scatter', mode = 'lines', line = list(dash = 'dash', color = 'red'),
                                name = 'resultUserProv Threshold')
          }
          if ("lod" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(rep(resultUserProv_LOD, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
                                type = 'scatter', mode = 'markers', marker = list(symbol = 'circle', color = 'red', size = 4),
                                name = 'resultUserProv LOD')
          }
          if ("loq" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(rep(resultUserProv_LOQ, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
                                type = 'scatter', mode = 'markers', marker = list(symbol = 'triangle-up', color = 'red', size = 4),
                                name = 'resultUserProv LOQ')
          }
          if ("cq" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(rep(resultUserProv_Cq, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
                                type = 'scatter', mode = 'markers', marker = list(symbol = 'square', color = 'red', size = 4),
                                name = 'resultUserProv Cq')
          }
          if ("loglinear" %in% input$elements) {
            p <- p %>%
              plotly::add_trace(x = c(as.numeric(resultUserProv_LogLinear[1]), as.numeric(resultUserProv_LogLinear[1]), as.numeric(resultUserProv_LogLinear[2]), as.numeric(resultUserProv_LogLinear[2])),
                                y = c(0, y_max, y_max, 0),
                                type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(255, 182, 193, 0.2)',  # light red
                                name = 'resultUserProv Log Linear Area')
          }
        }

        # Layout configuration
        p <- p %>%
          plotly::layout(title = "qPCR Overview Plot",
                         xaxis = list(title = "Cycle Number", range = c(1, max(filtered_cycles))),
                         yaxis = list(title = "Absorbance", range = c(0, y_max)),
                         showlegend = FALSE)  # Hide the legend as checkboxes now control visibility

        p
      })


    }
  },ignoreInit = TRUE)


  ################## Standard Curve Reset Selections ###########################
  shiny::observeEvent(input$SC_reset, {

    filteredqStdCurveAnalysis$value <- filtered$value
    # Remove any unk data as this needs to be opt or ntc data for standard curves
    filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultSampleType != "unkn",,drop=FALSE]

    #Populate the pickers
    print("In the Standard Curve reset button")

    #Populate the pickers
    updatePickerInput(session, "SC_project_input",
                      choices = unique(filteredqStdCurveAnalysis$value$projectID),
                      selected =  NULL)

    updatePickerInput(session, "SC_assay_input",
                      choices = unique(filteredqStdCurveAnalysis$value$assayID),
                      selected =  NULL)

    updatePickerInput(session, "SC_run_input",
                      choices = unique(filteredqStdCurveAnalysis$value$resultRunID),
                      selected =  NULL)

    updatePickerInput(session, "SC_platform_input",
                      choices = unique(filteredqStdCurveAnalysis$value$resultPlatform),
                      selected =  NULL)

    updatePickerInput(session, "SC_machine_input",
                      choices = unique(filteredqStdCurveAnalysis$value$resultMachineID),
                      selected =  NULL)

    updatePickerInput(session, "SC_curve_input",
                      choices = unique(filteredqStdCurveAnalysis$value$resultStdCurveID),
                      selected =  NULL)

    updatePickerInput(session, "SC_react_input",
                      choices = unique(filteredqStdCurveAnalysis$value$resultReactID),
                      selected =  NULL)

    output$standardCurve_plot <- NULL

  },ignoreInit = TRUE)

  ############################ Standard Curve Data Selections ##################
  shiny::observeEvent(input$SC_Update, {

    print("SC_Update - Begin")

    SCSelectProjectID <- input$SC_project_input
    SCSelectAssayID <- input$SC_assay_input
    SCSelectRunID <- input$SC_run_input
    SCSelectPlatformID <- input$SC_platform_input
    SCSelectMachineID <- input$SC_machine_input
    SCSelectCurveID <- input$SC_curve_input
    SCSelectReactID <- input$SC_react_input

    if(!is.null(SCSelectProjectID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$projectID %in% SCSelectProjectID,,drop=FALSE]
    }
    if(!is.null(SCSelectAssayID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$assayID %in% SCSelectAssayID,,drop=FALSE]
    }
    if(!is.null(SCSelectRunID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultRunID %in% SCSelectRunID,,drop=FALSE]
    }
    if(!is.null(SCSelectPlatformID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultPlatform %in% SCSelectPlatformID,,drop=FALSE]
    }
    if(!is.null(SCSelectMachineID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultMachineID %in% SCSelectMachineID,,drop=FALSE]
    }
    if(!is.null(SCSelectCurveID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultStdCurveID %in% SCSelectCurveID,,drop=FALSE]
    }
    if(!is.null(SCSelectReactID)){
      filteredqStdCurveAnalysis$value <- filteredqStdCurveAnalysis$value[filteredqStdCurveAnalysis$value$resultReactID %in% SCSelectReactID,,drop=FALSE]
    }


    if(length(unique(filteredqStdCurveAnalysis$value$projectID))==1){
      updatePickerInput(session, "SC_project_input",
                        choices = unique(filteredqStdCurveAnalysis$value$projectID),
                        selected = unique(filteredqStdCurveAnalysis$value$projectID))
    }else if(!is.null(SCSelectProjectID)){
      updatePickerInput(session, "SC_project_input",
                        choices = unique(filteredqStdCurveAnalysis$value$projectID),
                        selected = SCSelectProjectID)
    }
    if(length(unique(filteredqStdCurveAnalysis$value$assayID))==1){
      updatePickerInput(session, "SC_assay_input",
                        choices = unique(filteredqStdCurveAnalysis$value$assayID),
                        selected = unique(filteredqStdCurveAnalysis$value$assayID))
    }else if(!is.null(SCSelectAssayID)){
      updatePickerInput(session, "SC_assay_input",
                        choices = unique(filteredqStdCurveAnalysis$value$assayID),
                        selected = SCSelectAssayID)
    }
    if(length(unique(filteredqStdCurveAnalysis$value$resultRunID))==1){
      updatePickerInput(session, "SC_run_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultRunID),
                        selected = unique(filteredqStdCurveAnalysis$value$resultRunID))
    }else if(!is.null(SCSelectRunID)){
      updatePickerInput(session, "SC_run_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultRunID),
                        selected = SCSelectRunID)
    }
    if(length(unique(filteredqStdCurveAnalysis$value$resultPlatform))==1){
      updatePickerInput(session, "SC_platform_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultPlatform),
                        selected = unique(filteredqStdCurveAnalysis$value$resultPlatform))
    }else if(!is.null(SCSelectPlatformID )){
      updatePickerInput(session, "SC_platform_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultPlatform),
                        selected = SCSelectPlatformID )
    }
    if(length(unique(filteredqStdCurveAnalysis$value$resultMachineID))==1){
      updatePickerInput(session, "SC_machine_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultMachineID),
                        selected = unique(filteredqStdCurveAnalysis$value$resultMachineID))
    }else if(!is.null(SCSelectMachineID)){
      updatePickerInput(session, "SC_machine_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultMachineID),
                        selected = SCSelectMachineID)
    }
    if(length(unique(filteredqStdCurveAnalysis$value$resultStdCurveID))==1){
      updatePickerInput(session, "SC_curve_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultStdCurveID),
                        selected = unique(filteredqStdCurveAnalysis$value$resultStdCurveID))
    }else if(!is.null(SCSelectCurveID )){
      updatePickerInput(session, "SC_curve_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultStdCurveID),
                        selected = SCSelectCurveID )
    }
    if(length(unique(filteredqStdCurveAnalysis$value$resultReactID))==1){
      updatePickerInput(session, "SC_react_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultReactID),
                        selected = unique(filteredqStdCurveAnalysis$value$resultReactID))
    }else if(!is.null(SCSelectReactID )){
      updatePickerInput(session, "SC_react_input",
                        choices = unique(filteredqStdCurveAnalysis$value$resultReactID),
                        selected = SCSelectReactID )
    }



    #BEGIN THE PLOTTING

    filteredqStdCurveAnalysisEndUpdate <<-filteredqStdCurveAnalysis$value

    if(nrow(unique(filteredqStdCurveAnalysis$value[,c("projectID", "assayID", "resultRunID", "resultPlatform", "resultMachineID", "resultStdCurveID")]))==1){

      print("In the populate the figure section")
      userdata <- data.frame(Cq = filteredqStdCurveAnalysis$value$resultUserProvCq, log_concentration = filteredqStdCurveAnalysisEndUpdate$resultTemplateConcInCopy)
      mdmaprdata <- data.frame(Cq = filteredqStdCurveAnalysis$value$mdmaprCq, log_concentration = filteredqStdCurveAnalysisEndUpdate$resultTemplateConcInCopy)

      if(input$mdmapr){
        clean <- mdmaprdata[mdmaprdata$log_concentration > 0, ]
        clean$log_concentration <- log10(clean$log_concentration)
        clean <- clean[!is.na(clean$Cq), ]
        clean <- mdmaprdata$clean
        cleanfit <- lm(Cq ~ log_concentration, data = clean)
        clean$predicted_Cq <- predict(cleanfit, newdata = clean)
        clean$residual <- clean$Cq - clean$predicted_Cq
        LOD<-filteredqStdCurveAnalysis$userStdCurveLOD95
        LOQ<-filteredqStdCurveAnalysis$userStdCurveLOQ35
        plottitle <- "MDMAPR"
      }
      if(input$resultUserProv){
        clean <- userdata[userdata$log_concentration > 0, ]
        clean$log_concentration <- log10(clean$log_concentration)
        clean <- clean[!is.na(clean$Cq), ]
        clean <- userdata$clean
        userfit <- lm(Cq ~ log_concentration, data = clean)
        clean$predicted_Cq <- predict(userfit, newdata = clean)
        clean$residual <- clean$Cq - clean$predicted_Cq
        LOD<-filteredqStdCurveAnalysis$resultUserProvLOD
        LOQ<-filteredqStdCurveAnalysis$resultUserProvLOQ
        plottitle <- "User"
      }

      output$plot_output <- renderUI({
          userClean <- clean$userClean
          userfit <- lm(Cq ~ log_concentration, data = userClean)
          userClean$predicted_Cq <- predict(userfit, newdata = userClean)
          userClean$residual <- userClean$Cq - userClean$predicted_Cq

          p_user <- ggplot(userClean, aes(x = log_concentration, y = Cq, color = residual)) +
            geom_point() +
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "lightblue") +
            scale_color_gradientn(colors = color_palette) +
            labs(title = paste0(plottitle, "Source Data"),
                 x = "Log Concentration",
                 y = "Cq Values",
                 color = "Residuals") +
            theme_minimal()
      })
#
# SCLogData <- log(filteredqStdCurveAnalysisEndUpdate$resultTemplateConcInCopy)
# userdata <- data.frame(Cq = filteredqStdCurveAnalysisEndUpdate$resultUserProvCq, log_concentration = SCLogData)
# mdmaprdata <- data.frame(Cq = filteredqStdCurveAnalysisEndUpdate$mdmaprCq, log_concentration = SCLogData)
#
# print("Populate figure - 1")
#
#       SCLogData <- log(filteredqStdCurveAnalysis$value$resultTemplateConcInCopy)
#       userdata <- data.frame(Cq = filteredqStdCurveAnalysis$value$resultUserProvCq, log_concentration = SCLogData)
#       mdmaprdata <- data.frame(Cq = filteredqStdCurveAnalysis$value$mdmaprCq, log_concentration = SCLogData)
#
# print("Populate figure - 2")
#
#       # Remove rows with non-numeric or infinite values in the log_concentration column
#       userClean <- suppressWarnings(userdata[!is.na(as.numeric(userdata$log_concentration)) & is.finite(as.numeric(userdata$log_concentration)), ,drop = FALSE])
#       userClean <- suppressWarnings(userClean[!is.na(as.numeric(userClean$Cq)) & is.finite(as.numeric(userClean$Cq)), ,drop = FALSE])
#       mdmaprClean <- suppressWarnings(mdmaprdata[!is.na(as.numeric(mdmaprdata$log_concentration)) & is.finite(as.numeric(mdmaprdata$log_concentration)), ,drop = FALSE])
#       mdmaprClean <- suppressWarnings(mdmaprClean[!is.na(as.numeric(mdmaprClean$Cq)) & is.finite(as.numeric(mdmaprClean$Cq)), ,drop = FALSE])
#
# print("Populate figure - 3")
#
#       # Perform linear regression
#       userfit <- lm(as.numeric(Cq) ~ as.numeric(log_concentration), data = userClean)
#       mdmaprfit <- lm(as.numeric(Cq) ~ as.numeric(log_concentration), data = mdmaprClean)
#
# print("Populate figure - 4")
#
#       # Predict Cq values and calculate residuals
#       userClean$predicted_Cq <- predict(userfit, newdata = userClean)
#       userClean$residual <- userClean$Cq - userClean$predicted_Cq
#
# print("Populate figure - 5")
#
#       # Normalize residuals for color mapping
#       usermin_residual <- min(userClean$residual)
#       usermax_residual <- max(userClean$residual)
#       userClean$normalized_residual <- (userClean$residual - usermin_residual) / (usermax_residual - usermin_residual)
#
# print("Populate figure - 6")
#
#       # Predict Cq values and calculate residuals
#       mdmaprClean$predicted_Cq <- predict(mdmaprfit, newdata = mdmaprClean)
#       mdmaprClean$residual <- mdmaprClean$Cq - mdmaprClean$predicted_Cq
#
# print("Populate figure - 7")
#
#       # Normalize residuals for color mapping
#       mdmaprmin_residual <- min(mdmaprClean$residual)
#       mdmaprmax_residual <- max(mdmaprClean$residual)
#       mdmaprClean$normalized_residual <- (mdmaprClean$residual - mdmaprmin_residual) / (mdmaprmax_residual - mdmaprmin_residual)
#
# print("Populate figure - 8")
#
#       #the LOD and LOQ
#       userLOD_Cq <- resultUserProvLOQ
#       userLOQ_Cq <- resultUserProvLOD
#       mdmaprLOD_Cq <- mdmaprStdCurveLOD95
#       mdmaprLOQ_Cq <- mdmaprStdCurveLOQ35
#
# print("Populate figure - 9")

      # Define a color palette
      color_palette <- colorRampPalette(brewer.pal(9, "RdYlBu"))(100)
#
#       output$standardCurve_plot <- renderPlotly({
#         #
#         # # Create a ggplot with raw residuals
#         # p <- ggplot(data, aes(x = log_concentration, y = Cq)) +
#         #   geom_point(aes(color = residual)) +
#         #   scale_color_gradientn(colors = color_palette) +  # Using a more sophisticated color gradient
#         #   geom_line(data = average_residuals, aes(x = log_concentration, y = avg_residual), color = "black", linetype = "dashed") +
#         #   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#         #   geom_vline(xintercept = log10(LOD_Cq), linetype = "dashed", color = "red") +
#         #   geom_vline(xintercept = log10(LOQ_Cq), linetype = "dashed", color = "green") +
#         #   labs(title = "Standard Curve with LOD and LOQ\n(Raw Residuals Color Scheme)",
#         #        x = "Log Concentration",
#         #        y = "Cq Values",
#         #        color = "Residuals") +
#         #   theme_minimal()
#         #
#         # # Convert to interactive plotly plot
#         # ggplotly(p, tooltip = c("x", "y", "color"))
#
#       })


      #
      # print("Here in the SC_plot_data section her is SC_plot_data$standardConc - 1")
      # print(SC_plot_data$standardConc)
      #
      #           #Change user provided Cq to numeric value to numeric
      # #          SC_plot_data$systemCalculatedCqValue <- as.numeric(SC_plot_data$mdmaprCq)
      #           SC_plot_data$systemCalculatedCqValue <- as.numeric(as.data.frame(SC_filtered())$mdmaprCq)
      # print("SC_Plot - 3")
      # #          SC_plot_data$systemCalculatedLOQ<- as.numeric(SC_plot_data$mdmaprLOD)
      #           SC_plot_data$systemCalculatedLOQ<- as.numeric(as.data.frame(SC_filtered())$mdmaprLOQ)
      # print("SC_Plot - 4")
      # #          SC_plot_data$systemCalculatedLOD<- as.numeric(SC_plot_data$systemCalculatedLOD)
      #           SC_plot_data$systemCalculatedLOD<- as.numeric(as.data.frame(SC_filtered())$mdmaprLOD)
      # print("SC_Plot - 5")
      #
      #           #Add column with residual values to data set
      #           regression_line <- lm(as.numeric(systemCalculatedCqValue) ~ as.numeric(standardConc), SC_plot_data)
      #           SC_plot_data$Residual <- abs(residuals(regression_line))
      #
      #
      #
      #
      #




      # The selected data


    #
    #   absorbance <- filteredqPCRDataOverview$value[,grep("Cycle", names(filteredqPCRDataOverview$value), value = TRUE)]
    #
    #   cycles <- 1:length(absorbance)
    #
    #   # First set of thresholds
    #   mdmapr_Thres <- as.numeric(as.character(filteredqPCRDataOverview$value$mdmaprThres[1]))
    #   mdmapr_LOD <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$mdmaprStdCurveLineEq[1],filteredqPCRDataOverview$value$mdmaprStdCurveLOD95[1])))
    #   mdmapr_LOQ <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$mdmaprStdCurveLineEq[1],filteredqPCRDataOverview$value$mdmaprStdCurveLOQ35[1])))
    #   mdmapr_Cq <- as.numeric(as.character(filteredqPCRDataOverview$value$mdmaprCq[1]))
    #   mdmapr_LogLinear <- as.character(filteredqPCRDataOverview$value$mdmaprLogLinear[1])
    #   mdmapr_LogLinear <- as.numeric(unlist(strsplit(mdmapr_LogLinear, ":")))
    #
    #   # Second set of thresholds
    #   resultUserProv_Thres <- as.numeric(as.character(filteredqPCRDataOverview$value$resultUserProvThres[1]))
    #   resultUserProv_LOD <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$userStdCurveLineEq[1],filteredqPCRDataOverview$value$resultUserProvLOD[1])))
    #   resultUserProv_LOQ <- as.numeric(as.character(solve_for_x(filteredqPCRDataOverview$value$userStdCurveLineEq[1],filteredqPCRDataOverview$value$resultUserProvLOQ[1])))
    #   resultUserProv_Cq <- as.numeric(as.character(filteredqPCRDataOverview$value$resultUserProvCq[1]))
    #   resultUserProv_LogLinear <- as.character(filteredqPCRDataOverview$value$userLogLinear[1])
    #   resultUserProv_LogLinear <- as.numeric(unlist(strsplit(resultUserProv_LogLinear, ":")))
    #
    #   # Determine the last non-NA value index
    #   last_non_na_index <- which.max(is.na(absorbance)) - 1
    #   if (last_non_na_index == 0) last_non_na_index <- length(absorbance)  # handle case with no NAs
    #
    #   # Filter data up to the first NA
    #   filtered_absorbance <- as.numeric(as.character(absorbance[1:last_non_na_index]))
    #   filtered_cycles <- as.numeric(as.character(cycles[1:last_non_na_index]))
    #
    #   # Maximum value for y-axis
    #   y_max <- max(filtered_absorbance, na.rm = TRUE)
    #
    #   # Output plot
    #   output$qPCROverviewPlot <- plotly::renderPlotly({
    #     # Initialize plot
    #     p <- plotly::plot_ly()
    #
    #     # Add raw absorbance data
    #     p <- p %>%
    #       plotly::add_trace(x = filtered_cycles, y = filtered_absorbance, type = 'scatter', mode = 'markers',
    #                         marker = list(color = 'black', size = 4),
    #                         name = "Raw Absorbance")
    #
    #     # Add mdmapr data if checkbox is checked
    #     if ("mdmapr" %in% input$groups) {
    #       if ("thres" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(min(filtered_cycles), max(filtered_cycles)), y = rep(mdmapr_Thres, 2),
    #                             type = 'scatter', mode = 'lines', line = list(dash = 'dash', color = 'blue'),
    #                             name = 'mdmapr Threshold')
    #       }
    #       if ("lod" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(rep(mdmapr_LOD, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
    #                             type = 'scatter', mode = 'markers', marker = list(symbol = 'circle', color = 'blue', size = 4),
    #                             name = 'mdmapr LOD')
    #       }
    #       if ("loq" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(rep(mdmapr_LOQ, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
    #                             type = 'scatter', mode = 'markers', marker = list(symbol = 'triangle-up', color = 'blue', size = 4),
    #                             name = 'mdmapr LOQ')
    #       }
    #       if ("cq" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(rep(mdmapr_Cq, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
    #                             type = 'scatter', mode = 'markers', marker = list(symbol = 'square', color = 'blue', size = 4),
    #                             name = 'mdmapr Cq')
    #       }
    #       if ("loglinear" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(as.numeric(mdmapr_LogLinear[1]), as.numeric(mdmapr_LogLinear[1]), as.numeric(mdmapr_LogLinear[2]), as.numeric(mdmapr_LogLinear[2])),
    #                             y = c(0, y_max, y_max, 0),
    #                             type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(173, 216, 230, 0.2)',  # light blue
    #                             name = 'mdmapr Log Linear Area')
    #       }
    #     }
    #
    #     # Add resultUserProv data if checkbox is checked
    #     if ("resultUserProv" %in% input$groups) {
    #       if ("thres" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(min(filtered_cycles), max(filtered_cycles)), y = rep(resultUserProv_Thres, 2),
    #                             type = 'scatter', mode = 'lines', line = list(dash = 'dash', color = 'red'),
    #                             name = 'resultUserProv Threshold')
    #       }
    #       if ("lod" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(rep(resultUserProv_LOD, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
    #                             type = 'scatter', mode = 'markers', marker = list(symbol = 'circle', color = 'red', size = 4),
    #                             name = 'resultUserProv LOD')
    #       }
    #       if ("loq" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(rep(resultUserProv_LOQ, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
    #                             type = 'scatter', mode = 'markers', marker = list(symbol = 'triangle-up', color = 'red', size = 4),
    #                             name = 'resultUserProv LOQ')
    #       }
    #       if ("cq" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(rep(resultUserProv_Cq, length(filtered_cycles))), y = seq(0, floor(y_max), by = floor(y_max) / (length(filtered_cycles)-1)),
    #                             type = 'scatter', mode = 'markers', marker = list(symbol = 'square', color = 'red', size = 4),
    #                             name = 'resultUserProv Cq')
    #       }
    #       if ("loglinear" %in% input$elements) {
    #         p <- p %>%
    #           plotly::add_trace(x = c(as.numeric(resultUserProv_LogLinear[1]), as.numeric(resultUserProv_LogLinear[1]), as.numeric(resultUserProv_LogLinear[2]), as.numeric(resultUserProv_LogLinear[2])),
    #                             y = c(0, y_max, y_max, 0),
    #                             type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(255, 182, 193, 0.2)',  # light red
    #                             name = 'resultUserProv Log Linear Area')
    #       }
    #     }
    #
    #     # Layout configuration
    #     p <- p %>%
    #       plotly::layout(title = "qPCR Overview Plot",
    #                      xaxis = list(title = "Cycle Number", range = c(1, max(filtered_cycles))),
    #                      yaxis = list(title = "Absorbance", range = c(0, y_max)),
    #                      showlegend = FALSE)  # Hide the legend as checkboxes now control visibility
    #
    #     p
    #   })
    #
    #
    }











  },ignoreInit = TRUE)














#
# ################### Standard Curve Design page #################################
#
#
# ################### Standard Curve File Validation #############################
#
#
#
# #First when the Standard Curve Analysis tab is clicked then...
#
# #  If the tab is clocked check the uploaded_data. if it is Initial then don't do anything,
# # if it is anything else then check to see if there are records that are opt using...
# #  as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt", ]
#
# # if there are records that are opt then fill in the tables for the standard curve analysis
#
#
#
#
#
#
#     #Place an observe here if the tab for the SC analysis is open
#
#     observe({

# THIS input name changed to sidebarMenu
#       if(input$tab_being_displayed == "stdCurve") {
#
#
#
#
#
#
#
#     print("Here SC - 1")
#     #Update SC assay list
#     SC_assay_input_list <- reactive({
#       suppressWarnings(if(uploaded_data$value!="Initial"){
#   print("In the SC_assay_input_list reactive value and here is what is returned...")
#   print(unique(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt", "assayName"]))
#         return(unique(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt", "assayName"]))
#       }else{
#   print("In the SC_assay_input_list else returning NULL")
#         return(NULL)
#       })
#     })
#
#     observe({
#       updatePickerInput(session, "SC_assay_input",
#                         choices = SC_assay_input_list(),
#                         selected = SC_assay_input_list() )
#     })
#     print("Here SC - 2")
#     #Update SC assay list
#     SC_machine_input_list <- reactive({
#   print("In the SC_machine_input_list reactive value and here is what is returned...")
#   print(unique(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt", "resultPlatform"]))
#       suppressWarnings(if(uploaded_data$value!="Initial"){
#         return(unique(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt", "resultPlatform"]))
#       }else{
#         return(NULL)
#       })
#     })
#
#     observe({
#       updatePickerInput(session, "SC_machine_input",
#                         choices = SC_machine_input_list(),
#                         selected = SC_machine_input_list() )
#     })
#     print("Here SC - 3")
#     #Update SC assay list
#     SC_project_input_list <- reactive({
#       suppressWarnings(if(uploaded_data$value!="Initial"){
#         return(unique(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt", "projectID"]))
#       }else{
#         return(NULL)
#       })
#     })
#
#     observe({
#       updatePickerInput(session, "SC_project_input",
#                         choices = SC_project_input_list(),
#                         selected = SC_project_input_list() )
#     })
#
#
#
#     SC_well_list <- reactive({
#   print("In the SC_well_list reactive value and here is what is returned...")
#   print(unique(row.names(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt",])))
#       suppressWarnings(if(uploaded_data$value!="Initial"){
#         return(unique(row.names(as.data.frame(uploaded_data$value)[as.data.frame(uploaded_data$value)$resultSampleType == "opt",])))
#       }else{
#         return(NULL)
#       })
#     })
#
#     observe({updatePickerInput(session,"SC_wells",
#                                choices = SC_well_list(),
#                                selected = SC_well_list())})
#
#     SC_filtered <- reactive({
#
#       print("At the top of the SC_filtered reactive")
#
#       suppressWarnings(if (uploaded_data$value != "Initial") {
#
#         print("Here at top of SC_Filtered")
#
#         #Get the data from the initial upload which are standard curve data
#         SC_data_final <- as.data.frame(uploaded_data$value)
# #        SC_data_final <- SC_data_final[SC_data_final$resultSampleType == "opt",,drop=FALSE]
# #        SC_data_final <- SC_data_final[,base::c("assayName", "assayID", "resultPlatform", "projectID",
# #                        "resultReactID", "resultWellLoc", "resultTemplateConcInCopy", "resultUserProvThres", "resultUserProvCq", "mdmaprThres", "mdmaprCq") ,drop=FALSE]
#         SC_data_finalGlobalA<<-SC_data_final
#         print("SC_filtered reactive - 1")
#         #Reduce the data by the assay name
#         SC_data_final<-SC_data_final[SC_data_final$assayName %in% input$SC_assay_input,,drop=FALSE]
#         SC_data_finalGlobalB<<-SC_data_final
#         #Reduce the data by the assay name
#         SC_data_final<-SC_data_final[SC_data_final$resultPlatform %in% input$SC_machine_input,,drop=FALSE]
#         print("SC_filtered reactive - 2")
#         SC_data_finalGlobalC<<-SC_data_final
#         #Reduce the data by the assay name
#         SC_data_final<-SC_data_final[SC_data_final$projectID %in% input$SC_project_input,,drop=FALSE]
#         print("SC_filtered reactive - 3")
#         SC_data_finalGlobalD<<-SC_data_final
#         SC_data_final<-SC_data_final[row.names(SC_data_final) %in% input$SC_wells,,drop=FALSE]
#         #SC_data_finalGlobalE<<-SC_data_final
#
#       }else{
#
#   print("here in the else fo the SC_Filtered")
#    #      return("Initial")
#         return(NULL)
#       })#End of if else
#
#     })
#
#
#   observe({
#
#THIS input name changed to sidebarMenu
#     if(input$tab_being_displayed == "stdCurve"){
#
#       suppressWarnings(if (nrow(as.data.frame(SC_filtered())) >0) {
#
#         SC_table <- as.data.frame(SC_filtered())[as.data.frame(SC_filtered())$resultSampleType == "opt",,drop=FALSE]
#         SC_table <- SC_table[,base::c("assayName", "assayID", "resultPlatform", "projectID",
#                                 "resultReactID", "resultWellLoc", "resultTemplateConcInCopy", "resultUserProvThres", "resultUserProvCq", "mdmaprThres", "mdmaprCq") ,drop=FALSE]
#
#
#         #Dynamic data table with SC data
#         output$SC_overview_table <- DT::renderDT({
#
#           #The following line creates the data table using the filtered data but removing any columns that have no data
#   #        datatable(as.data.frame(SC_filtered()),
#   #        DT::datatable(as.data.frame(SC_filtered()),
#           DT::datatable(SC_table,
#                     options = list(pageLength = 50,
#                                    scrollX = TRUE, scrollY = TRUE
#                     ))
#         })
#       }
#     )}#End of if filtered()
#   })# end of the observe
#
#
#
#
#       ################### Standard Curve Re-calibration based on wells selected ######
#        observeEvent(input$std_recalib, isolate({
#
#          output$standardCurve_plot <- renderPlotly({
#    #        req(())
#            #filter data based on well selected
#    #        SC_plot_data <- ()
# #           SC_plot_data <- SC_plot_data[SC_plot_data$wellLocation==input$SC_wells, ]
# #           SC_plot_data <- as.data.frame(SC_filtered())
#
#            #Remove control records
# #           SC_plot_data <- control_records_to_remove(SC_plot_data)
#
#
#
#            # #Return popup message regarding uploaded standard curve fluorescence file.
#            #
#            # control_records_to_remove <- function(meta_data) {
#            #   if (length(which(grepl("Y", toupper(meta_data$control)))) != 0)
#            #   {return (meta_data[-base::c(which(grepl("Y", toupper(meta_data$control)))), ])}
#            #
#            #   else
#            #   {return(meta_data)}
#            # }
#
#
#
# SC_filtered_CalibrateGlobalA<<-SC_filtered()
#
#   print("At the beginning of the plotting here are the concentration data")
#   print(as.data.frame(SC_filtered())$resultTemplateConcInCopy)
#
#           #Change standard concentration value to numeric and then take log value
#           #SC_plot_data$standardConc <- as.numeric(SC_plot_data$standardConc)
# #          SC_plot_data <- data.frame()
# print("SC_Plot - 1")
# print(as.numeric(as.data.frame(SC_filtered())$resultTemplateConcInCopy))
# #          SC_plot_data$standardConc <- as.numeric(as.data.frame(SC_filtered())$resultTemplateConcInCopy)
#           SC_plot_data <- data.frame(resultWellLoc=as.data.frame(SC_filtered())$resultWellLoc)
#           SC_plot_data$standardConc <- as.numeric(as.data.frame(SC_filtered())$resultTemplateConcInCopy)
# print("SC_Plot - 2")
# SC_plot_dataGlobalA<<-SC_plot_data
#
#           SC_plot_data$standardConc <- log(SC_plot_data$standardConc)
#
# print("Here in the SC_plot_data section her is SC_plot_data$standardConc - 1")
# print(SC_plot_data$standardConc)
#
#           #Change user provided Cq to numeric value to numeric
# #          SC_plot_data$systemCalculatedCqValue <- as.numeric(SC_plot_data$mdmaprCq)
#           SC_plot_data$systemCalculatedCqValue <- as.numeric(as.data.frame(SC_filtered())$mdmaprCq)
# print("SC_Plot - 3")
# #          SC_plot_data$systemCalculatedLOQ<- as.numeric(SC_plot_data$mdmaprLOD)
#           SC_plot_data$systemCalculatedLOQ<- as.numeric(as.data.frame(SC_filtered())$mdmaprLOQ)
# print("SC_Plot - 4")
# #          SC_plot_data$systemCalculatedLOD<- as.numeric(SC_plot_data$systemCalculatedLOD)
#           SC_plot_data$systemCalculatedLOD<- as.numeric(as.data.frame(SC_filtered())$mdmaprLOD)
# print("SC_Plot - 5")
#
#           #Add column with residual values to data set
#           regression_line <- lm(as.numeric(systemCalculatedCqValue) ~ as.numeric(standardConc), SC_plot_data)
#           SC_plot_data$Residual <- abs(residuals(regression_line))
#
#           #Code to get R squared
#           #Adapted from: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
#           # Code to get equation of the line and R-squared
#           # Adapted from: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA
#
#           lm_eq <- function(df){
#
#             model1 <- lm(systemCalculatedCqValue ~ standardConc, df, na.action=na.exclude)
#
#             b = format(unname(coef(model1)[1]), digits = 2)
#
#             mx = paste("(", format(unname(coef(model1)[2]), digits = 2), "x", ")",  sep = "")
#
#             r2 = format(summary(model1)$r.squared, digits = 3)
#
#             equation_of_line <- paste("y", " = ", mx, " + ", b, ",   ", "R-squared", " = ", r2,  sep = "")
#
#             return (equation_of_line)
#
#           }
#
#
#
#           print(
#
#             ggplotly(height = 700,
#
#                      ggplot(data = SC_plot_data, aes(x = standardConc,
#                                                      y = systemCalculatedCqValue,
#                                                      color = Residual)) +
#
#                        geom_point(size = 10, alpha = .3) +
#
#                        geom_smooth(method = "lm",
#                                    se = FALSE,
#                                    alpha = .15,
#                                    color = 'black',
#                                    formula = y ~ x) +
#
#                        geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOD),
#                                   color = "#ea5f94",
#                                   linetype="dashed") +
#                        #
#                        geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOQ),
#                                   color = "#0178A1",
#                                   linetype="dotted") +
#
#                        geom_text(aes(x= log(systemCalculatedLOD),
#                                      label="LOD",
#                                      y= mean(systemCalculatedCqValue)),
#                                  colour="#ea5f94",
#                                  angle=90,
#                                  vjust = 1.2,
#                                  size=5) +
#
#                        geom_text(aes(x= log(systemCalculatedLOQ),
#                                      label="LOQ",
#                                      y= mean(systemCalculatedCqValue)*1.2),
#                                  colour="#0178A1",
#                                  angle=90,
#                                  vjust = 1.2,
#                                  size=5) +
#
#                        geom_text(aes(label=resultWellLoc)) +
#
#
#                        xlab("log DNA Copy Number") +
#
#                        ylab("System Calculated Cq") +
#
#                        theme_minimal() +
#
#                        scale_color_gradient(low = "#fbd300", high = "#0000ff") +
#
#                        theme( axis.title.x = element_text( size=13),
#                               axis.title.y = element_text( size=13),
#                               axis.text.x = element_text(size=12),
#                               axis.text.y = element_text(size=12),
#                               plot.title = element_text(hjust = 0.5, size=18)) +
#
#                        # ggtitle(paste0("Standard Curve for ", input$SC_project_input,  "\n",
#                        #                lm_eq(SC_plot_data)))
#                        ggtitle(paste0("Standard Curve ",
#                                       lm_eq(SC_plot_data)))
#
#             ) %>%
#
#               layout(margin = list(l=50, r=50, b=100, t=100, pad=4),
#                      annotations = list(x = 1.1, y = -0.17,
#                                         text = "Source: MDMAPR-CC-BY",
#                                         showarrow = F,
#                                         xref='paper',
#                                         yref='paper',
#                                         font=list(size=12, color="darkblue"))))
#         })
#       }))
#
#     }
#   })#End of the observe if the stdCurve tab is open on line 1178


#
#     # ################### Standard Curve Table Output ################################
#     # output$SC_overview_table  <- renderDataTable({
#     #
#     #   data <- ()[ , -base::c(1, 2, 3, 4, 102)]
#     #
#     #   datatable(data,
#     #             options = list(scrollX = TRUE,
#     #                            autoWidth = TRUE,
#     #                            columnDefs = list(list(width = '500px', targets = c(84)))))})
#     #
#     #
#     #
#     #
#     ################### Low Quant eDNA LOD Method ##################################
#     output$lowquant_standardCurve_plot <- renderPlot({
# #      req(())
#
# #      DAT <- ()[, c("standardCurveName", "runRecordedBy", "systemCalculatedCqValue", "standardConc")]
#       # rename the columns to match the calculation
#       colnames(DAT) <- c("Target", "Lab", "Cq", "SQ")
#
#       ## Ensure data is in the proper format:
#       DAT$Target <- as.factor(DAT$Target)
#       DAT$Lab <- as.factor(DAT$Lab)  #ML
#       DAT$Cq <- suppressWarnings(as.numeric(as.character(DAT$Cq))) #Non-numerical values (i.e. negative wells) will be converted to NAs
#       DAT$SQ <- suppressWarnings(as.numeric(as.character(DAT$SQ))) #Non-numerical values (i.e. NTC) will be converted to NAs
#       # by MDMAPR Definitions, if a Cq value is 40, there is no amplification and should be converted to NA
#       DAT$Cq[which(DAT$Cq==40)] <- NA
#
#       # setting all negative controls to 0
#       DAT$SQ[is.na(DAT$SQ)] <- 0
#       DAT.df <- data.frame(DAT)
#
#       # compute poisson estimates
#       DAT.Tar.SQ <-  DAT.df %>%
#         group_by(Target, SQ) %>%
#         dplyr::summarise(detect=sum(!is.na(Cq)), n=n(),  Cqmean=mean(Cq, na.rm=TRUE),
#                          Lab=Lab[1])
#       DAT.Tar.SQ <- droplevels(data.frame(DAT.Tar.SQ))
#       uLabs <- unique(DAT.Tar.SQ$Lab) #unique labs
#
#       DAT.Tar.SQ <- arrange(DAT.Tar.SQ, Lab, Target, SQ) #sort data by SQ in Target in Lab
#
#       ## Add variables to data set:  L10.SQ, phat, ...
#       DAT.Tar.SQ <- within(DAT.Tar.SQ, {
#         L10.SQ <- log10(SQ)
#         phat <- detect/n           #sample proportion detect
#         vphat <- phat*(1-phat)/n   #var of phat
#         lamhat <- -log(1-phat)
#         vlamhat <- phat/n/(1-phat)  #var of lamhat using the delta method
#         sdlamhat <- sqrt(vlamhat)   #sd of lamhat using the delta method
#         MElamhat <- 1.96*sdlamhat  #margin of error for lambda hat using delta method
#       }
#       )
#       ## All Targets and Labs **DO NOT DUPLICATE Target names over Labs!!
#       uLabs <- unique(DAT.Tar.SQ$Lab)
#       uTargets <- unique(DAT.Tar.SQ$Target)
#       nTargets <- length(uTargets)
#       uLabsTargets <- unique(DAT.Tar.SQ[,c('Lab','Target')])
#       uLabsTargets$Lab <- as.character(uLabsTargets$Lab)
#
#       #ensure ulabsTargets in same order as uTargets
#       uLabsTargets <- uLabsTargets[match(uLabsTargets$Target, uTargets),]
#       uLabsTargets.names <- apply(uLabsTargets, 1, paste, collapse=', ')
#
#       DAT.Tar.SQ <- within(DAT.Tar.SQ, {
#         CIexphat.lower <-  1 - qbeta(.975, n-detect+1, detect)  #exact phat bounds
#         CIexphat.upper <-  qbeta(.975, detect+1, n-detect)
#
#         ## Use transformed exact phat bounds
#         Lamhatex.Lower <- -log(1 - CIexphat.lower)
#         Lamhatex.Upper <- -log(1 - CIexphat.upper)
#       }
#       )
#       print("got here2")
#       nndetect <- vector("list", nTargets)
#       nrowTarget <- rep(0, length=nTargets)
#
#       for(i in 1:nTargets) {
#
#         print(paste0(nTargets, "is there a mistake"))
#
#         Target.dat <- subset(DAT.Tar.SQ, Target==uTargets[i])
#         print(Target.dat)
#         bSQ <- !is.na(Target.dat$phat)
#         lastSQ <- as.logical(cumprod(Target.dat$phat!=1 & bSQ))
#         ## removes first observations with SQ with phat=1 and larger SQs
#         Target.dat <- Target.dat[lastSQ,]
#         print(Target.dat)
#         print("with last sq")
#         nndetect[i] <- list(Target.dat )
#         nrowTarget[i] <- nrow(Target.dat)
#
#         print(nrow(nndetect[[i]]))
#
#         if(nrow(nndetect[[i]]) < 2) {print("we got here");next}
#
#         maxSQ <- max(Target.dat$SQ)
#         maxlamhat <- max(Target.dat$lamhat)
#
#         ######################### show this plot on the standard curve tab
#         print("got here 3")
#         plot(Target.dat$SQ, Target.dat$lamhat, xlog=TRUE, ylab='Lambda hat',
#              xlab='Starting copy number',
#              ylim=c(0, maxlamhat), xlim=c(0, maxSQ), main=uLabsTargets.names[i])
#         ## Transformed Exact CI
#         arrows(Target.dat$SQ, Target.dat$Lamhatex.Lower, Target.dat$SQ,
#                Target.dat$Lamhatex.Upper,
#                length=0.05, angle=90, code=3)
#         ## overlay simple regression line and R-squared
#         jlm <- lm(lamhat ~ SQ, data=Target.dat)
#         abline(jlm, col=2)
#         legend("topleft", paste('lm Rsq=',round(summary(jlm)$r.squared, 2)), bty="n")
#         print("got here 4")
#
#       }
#     })
#
#
#
#     ################### qPCR Data Overview page #####################################
#
#     ##Presence Absence table
#
#     # Filter options on data overview page
#     #Update assay list on page when additional file is uploaded
#     DA_assay_list <- reactive({
#
#       if (!is.null(uploaded_data$value)) {
#
#         data <- as.data.frame(uploaded_data$value)
#
#         assay_data <- append('None', as.character(unique(data$assayName)))
#
#         return(assay_data)}
#
#       else {
#         return(NULL)}
#     })
#
#     observe({updatePickerInput(session,
#                                "DA_assay_input",
#                                choices = DA_assay_list(),
#                                selected = 'nuBrook Trout TripleLock')})
#
#     #Update project list based on assay selection
#     DA_project_list <- reactive({
#
#       if (input$DA_machine_input != 'None') {
#
#         data <- as.data.frame(uploaded_data$value)
#
#         updated_list <- data[data$assayName == input$DA_assay_input, ]
#         updated_list <- updated_list[updated_list$runPlatform == input$DA_machine_input, ]
#         project_list <-  as.character(unique(updated_list$projectName))
#
#         return(project_list)}
#
#       else {return(NULL)}
#     })
#
#
#     observe({updatePickerInput(session,
#                                "DA_project_input",
#                                choices = append('None', DA_project_list()),
#                                selected = 'None') })
#
#
#     #Transform dataframe for presence/absence table based on filtered
#     presence_absence_table_data <- reactive({
#
#       if (!is.null(uploaded_data$value)) {
#
#         data <- uploaded_data$value
#         PA_filtered_data <- data[data$assayName == input$DA_assay_input, ]
#         PA_filtered_data <- PA_filtered_data[PA_filtered_data$runPlatform == input$DA_machine_input, ]
#         PA_filtered_data <- PA_filtered_data[PA_filtered_data$projectName == input$DA_project_input, ]
#
#       }
#     })
#
#     what_clr <- function(value) {
#       if (value >= input$cqValueCutoff)
#       {return ("#8FBACB")}
#
#       else
#       {return("#ffb14e")}
#     }
#
#
#     available_threshold <- function(value) {
#       if (value == "Unable to Determine Threshold")
#       {return ("#ffd700")}
#     }
#
#     # #Presence/absence table
#     observeEvent(input$submit, isolate ({
#
#       output$presence_absence_table <- reactable::renderReactable({
#         data <- as.data.frame(presence_absence_table_data())
#
#         prescence_abscence_table <- data[ , c("projectName", "runID", "extractName", "control", "geneSymbol", "runPlatform", "wellLocation", "userProvidedThresholdValue", "userProvidedCqValue", "systemCalculatedThresholdValue", "systemCalculatedCqValue" )]
#
#         reactable::reactable(prescence_abscence_table,
#
#                   #Table columns
#                   columns = list(
#
#                     projectName = colDef(name = "Project Name",align = "center", width = 300),
#                     runID = colDef(name = "Plate ID", align = "center"),
#                     extractName = colDef(name = "Sample Name", align = "center", width = 200),
#                     control = colDef(name = "Control", align = "center"),
#                     geneSymbol = colDef(name = "Gene", align = "center"),
#                     runPlatform = colDef(name = "Machine", align = "center"),
#                     wellLocation = colDef(name = "Well Location", align = "center", width = 200),
#
#                     userProvidedThresholdValue = colDef(name = "User Provided Threshold",
#                                                         align = "center",
#                                                         width = 300),
#
#                     # userProvidedCqValue = colDef(name = "User Provided Cq Value",
#                     #                              width = 250,
#                     #                              style = function(value) {
#                     #                                color  <- what_clr(value)
#                     #                                list(background = color)}),
#
#                     systemCalculatedThresholdValue = colDef(name = "System Calculated Threshold",
#                                                             width = 300,
#                                                             align = "center",
#                                                             style = function(value) {
#                                                               color  <- available_threshold(value)
#                                                               list(background = color)}),
#
#                     systemCalculatedCqValue = colDef(name = "System Calculated Cq Value",
#                                                      width = 250,
#                                                      style = function(value) {
#                                                        color  <- what_clr(value)
#                                                        list(background = color)})),
#
#                   #Filter each column by text
#                   filterable = TRUE,
#
#                   #Type in page number to jump to a page
#                   paginationType = "jump",
#
#                   #Minimum rows shown on page
#                   minRows = 20,
#
#                   #Number of rows to show
#                   defaultPageSize = 20,
#
#                   #Adding outline around cells
#                   outlined = TRUE,
#
#                   #Color every other row
#                   striped = TRUE,
#
#                   #Hover above row to highlight it
#                   highlight = TRUE,
#
#                   #Default record selected from table
#                   defaultSelected = 1,
#
#                   #Check box
#                   selection = "single",
#
#                   #Wrap text in column
#                   wrap = FALSE,
#
#                   theme = reactable::reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee",
#                                                                  boxShadow = "inset 2px 0 0 0 #ffa62d"))
#         )
#       })
#     }))
#     #
#     #
#     # ## Amplification plot
#     #
#     # #Get selected row for amplification plot
#     selected <- reactive(reactable::getReactableState("presence_absence_table", "selected"))
#     #
#     # #Created amplifcation plot based on selected well sample
#     #
#     observeEvent(input$submit, isolate ({
#
#       output$selected <-  renderPlotly({
#
#         #Created dataframe of filtered presence/absence data
#         data <- as.data.frame(presence_absence_table_data())[selected(), ]
#
#         #Create data frame for amplification curve
#         amp_curve_data <- na.omit(as.data.frame(t(data[ , c(18:87)])))
#         colnames(amp_curve_data) <- "Fluorescence"
#         amp_curve_data$cycles <- c(1:nrow(amp_curve_data))
#
#
#         #Created plot
#         print(
#           ggplotly(height = 700,
#
#                    ggplot(amp_curve_data, aes(x = cycles, y = as.numeric(Fluorescence))) +
#
#                      geom_point(aes(colour = "Absorbances") , size = 2) +
#
#                      geom_hline(aes(yintercept = as.numeric(data$userProvidedThresholdValue),
#                                     color = "User Provided Threshold"),
#                                 linetype="dashed", size = 1) +
#
#                      geom_hline(aes(yintercept = as.numeric(data$systemCalculatedThresholdValue),
#                                     color = "System Calculated Threshold"),
#                                 linetype="dotted", size = 1) +
#
#
#                      ggtitle(paste0( "Well ", data$wellLocation, " Amplification Curve")) +
#
#                      labs(x = " Cycle", y = "Absorbance") +
#
#                      theme_gray() +
#
#                      scale_colour_manual("",
#                                          breaks = c("Absorbances",
#                                                     "User Provided Threshold",
#                                                     "System Calculated Threshold"),
#                                          values = c("User Provided Threshold"="#ea5f94",
#                                                     "Absorbances"="#0000ff",
#                                                     "System Calculated Threshold"="#ff8600")) +
#
#                      theme(plot.title = element_text(hjust = 0.5, size=18),
#                            axis.title.x = element_text( size=13),
#                            axis.title.y = element_text( size=13),
#                            axis.text.x = element_text(size=12),
#                            axis.text.y = element_text(size=12),
#                            legend.text = element_text(size = 10),
#                            legend.background = element_rect(fill="lightblue")))  %>%
#
#             layout(legend = list(orientation = "h", x = 0.02, y = -0.16), #legend position
#                    margin = list(l=50, r=60, b=140, t=100, pad=4),
#                    annotations = list(x = 1, y = -0.31,
#                                       text = "Source: MDMAPR-CC-BY",
#                                       showarrow = F,
#                                       xref='paper',
#                                       yref='paper',
#                                       font=list(size=12,
#                                                 color="darkblue"))))})
#     }))
#
#     #
#     #    ## welcome page (Create downloadable metadata template) -----
#     #
#     #    #Created  metadata template for users to download
#     #    project_sheet <- data.frame(matrix(ncol = 24, nrow = 1))
#
#     #    colnames(project_sheet) <- c("projectID", "projectCreationDate","projectName","projectRecordedBy","projectOwner","projectContactEmail","projectDescription","InstitutionID","projectDataNotes","geographicRegionID", "continent","country", "stateProvince","municipality","siteID", "locality","estimatedPerimeter","estimatedSurfaceArea(m2)","siteType","siteLength(m2)", "stationID","stationName", "decimalLongitude", "decimalLatitude")
#
#
#     #    replicate_sheet <- data.frame(matrix(ncol = 55, nrow = 1))
#     #    colnames(replicate_sheet) <- c("replicateID", "stationID", "collectorName","replicateName","collectionDate","collectionTime","storageID","DateOfStorage","methodOfStorage","minimumElevationInMeters","maximumElevationInMeters","verbatimElevation","minimumDepthInMeters","maximumDepthInMeters","verbatimDepth","flowRate(m/s)", "filterType","filtrationDuration(mins)","volumeFiltered","processLocation","replicationNumber","riparianVegetationPercentageCover","dissolvedOxygen(mg/L)","waterTemperature(C)","pH","TSS(mg/L)","EC(uS/cm)","turbidity(NTU)","discharge","tide","chlorophyl","salinity(ppt)","contaminants(ng/g)","traceMetals(mg/kg)","organicContent(%)","microbialActivity","grainSize","replicateDataNotes", "extractID", "extractName","analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes","tubePlateID","frozen", "fixed","dnaStorageLocation","extractMethodOfStorage","dnaVolume","quantificationMethod", "concentration(ng/ul)")
#
#
#     #    assay_sheet <- data.frame(matrix(ncol = 30, nrow = 1))
#     #    colnames(assay_sheet) <- c( "assayID", "establishmentMeans","assayName","assayOwnership","assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol","dilutions", "replicates", "primerR", "primerF", "probe","ampliconLength (bp)", "probeFluorescentTag", "dye(s)","quencher","probeModification", "taxonID", "kingdom","phylum","class","order","family", "genus", "subgenus", "species", "vernacularName","organismScope")
#
#     #    results_sheet <- data.frame(matrix(ncol = 29, nrow = 1))
#     #    colnames(results_sheet) <- c("resultID","assayID", "extractID", "wellLocation","sampleName", "copyNumber", "control", "userProvidedThresholdValue", "userProvidedCqValue", "runID", "runRecordedBy", "runDate", "runTime","runPlatform","machineID", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")
#
#     #    standardCurveResults_sheet <- data.frame(matrix(ncol = 36, nrow = 1))
#     #    colnames(standardCurveResults_sheet ) <- c("SCresultID","wellLocation","sampleName", "copyNumber", "control","standardConc",   "userProvidedThresholdValue", "userProvidedCqValue","runID", "runRecordedBy", "runDate", "runTime", "runPlatform","machineID", "standardCurveID","assayID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes", "LOD","LOQ", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")
#
#
#     #    data_list <- (list( project_Table = project_sheet,
#     #                        replicate_Table = replicate_sheet,
#     #                        assay_Table = assay_sheet,
#     #                        results_Table = results_sheet,
#     #                        standardCurveResults_Table = standardCurveResults_sheet))
#
#     #    output$downloadTemplate <- downloadHandler(
#     #      filename = 'MDMAPR_metadata_template.xlsx',
#     #      content = function(file) {write_xlsx(data_list, file)})
#
#
#
#
#     ################### Data Modelling page ########################################
#
#     output$report <- downloadHandler(
#       # For PDF output, change this to "report.pdf"
#       filename = paste("report", sep = ".", "html"),
#       content = function(file) {
#         # Copy the report file to a temporary directory before processing it, in
#         # case we don't have write permissions to the current working dir (which
#         # can happen when deployed).
#         tempReport <- file.path(tempdir(), "report.Rmd")
#         file.copy("report.Rmd", tempReport, overwrite = TRUE)
#
#         # Set up parameters to pass to Rmd document
#         params <- list(n = input$selectData)
#
#         # Knit the document, passing in the `params` list, and eval it in a
#         # child of the global environment (this isolates the code in the document
#         # from the code in this app).
#         rmarkdown::render(tempReport,
#                           output_file = file,
#
#                           envir = new.env(parent = globalenv()))
#       }
#     )
#
#     # initialize the reactive value
#     InputDataset <- reactiveVal(NULL)
#
#     # read in the uploaded csv file, also removing all columns that are all NA
#     observeEvent(input$submit_modelling,{
#       modelling_data <- as.data.frame(read.csv(input$model_data$datapath))
#       # need to convert all NULL to NA
#       modelling_data[modelling_data=="NULL"] <- NA
#       modelling_data <- modelling_data[colSums(!is.na(modelling_data)) > 0]
#
#       # convert all character variables into factor
#       modelling_data[sapply(modelling_data, is.character)] <- lapply(modelling_data[sapply(modelling_data, is.character)],
#                                                                      as.factor)
#       # remove the columns that have only one value throughout.
#       modelling_data <- modelling_data[vapply(modelling_data, function(x) length(unique(x)) > 1, logical(1L))]
#       return(InputDataset(modelling_data))
#
#     })
#
#
#     InputDataset_model <- reactive({
#       if (is.null(input$SelectX)) {
#         dt <- as.data.frame(InputDataset())
#       }
#       else{
#         dt <- as.data.frame(InputDataset()[, c(input$SelectX)])
#       }
#
#     })
#
#
#     output$Xvarselection <- renderUI({
#       shinydashboard::box(
#         pickerInput("SelectX",
#                     "Select variables:",
#                     multiple = TRUE,
#                     choices = as.character(names(InputDataset_model())),
#                     selected=as.character(names(InputDataset_model())),
#                     width="200px"),
#         status = "primary",
#         width = "300px",
#         height = "200px",
#         solidHeader = TRUE,
#         title = "X variables")
#     })
#
#     #
#     output$SelectY <-  renderUI({
#       shinydashboard::box(
#         selectizeInput('SelectY',
#                        "Select variable to predict:",
#                        multiple = F,
#                        choices = as.character(names(InputDataset())),
#                        selected=1,
#                        width="200px"),
#         status = "primary",
#         width = "300px",
#         height = "200px",
#         solidHeader = TRUE,
#         title = "Y Variable")
#     })
#
#
#     # observe({
#     #   lstname <- names(InputDataset())
#     #   updateSelectInput(session = session,
#     #                     inputId = "SelectY",
#     #                     choices = lstname)
#     # })
#
#     splitSlider <- reactive({
#       input$Slider1 / 100
#     })
#     output$Summ <-
#       renderPrint(
#         stargazer(
#           InputDataset(),
#           type = "text",
#           title = "Descriptive statistics",
#           digits = 1,
#           out = "table1.txt"
#         )
#       )
#     output$Summ_old <- renderPrint(summary(InputDataset()))
#     output$structure <- renderPrint(str(InputDataset()))
#
#     set.seed(100)  # setting seed to reproduce results of random sampling
#     trainingRowIndex <-
#       reactive({
#         sample(1:nrow(InputDataset_model()),
#                splitSlider() * nrow(InputDataset_model()))
#       })# row indices for training data
#
#     trainingData <- reactive({
#       tmptraindt <- InputDataset_model()
#       tmptraindt[trainingRowIndex(), ]
#     })
#
#     testData <- reactive({
#       tmptestdt <- InputDataset_model()
#       tmptestdt[-trainingRowIndex(),]
#     })
#
#
#
#     output$cntTrain <-
#       shiny::renderText(paste("Train Data:", NROW(trainingData()), "records"))
#     output$cntTest <-
#       shiny::renderText(paste("Test Data:", NROW(testData()), "records"))
#
#     output$Data <- DT::renderDT(InputDataset())
#
#     # identify variables that are numeric for correlation matrix
#     numeric_model_input <- reactive({
#       nums <- unlist(lapply(InputDataset(), is.numeric))
#       return(InputDataset()[ , nums])
#     })
#
#     cormat <- reactive({
#       round(cor(numeric_model_input()), 1)
#     })
#     output$Corr <-
#       renderPlot(corrplot(
#         cormat(),
#         type = "lower",
#         order = "hclust",
#         method = "number"
#       ))
#
#     correlationMatrix <- reactive({
#       cor(numeric_model_input())
#       print(cor(numeric_model_input()))
#     })
#
#     # it's this line that causes problems
#     output$CorrMatrix <-
#       renderPrint(round(as.data.frame(correlationMatrix()), 4))
#     #
#
}
