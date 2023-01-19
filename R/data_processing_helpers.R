
df_formatting_by_target <- function(fluorescence_df){

  #Build a unique ID to mirror the metadata file unique ID
  fluorescence_df <- fluorescence_df[, c( "exp.id", "run.id", "react.id","fluor")]
  built_unique_ID<-paste0(fluorescence_df$exp.id,"|",fluorescence_df$run.id,"|", fluorescence_df$react.id)
  built_unique_ID<-paste0(fluorescence_df$exp.id,"_",fluorescence_df$run.id,"_", fluorescence_df$react.id)
  fluorescence_df<-as.data.frame(cbind(built_unique_ID,fluorescence_df$fluor))
  # subset the data with the columns we need for the raw fluorescence
  # split by position
  formatted_df <- do.call('data.frame', split(fluorescence_df, fluorescence_df$built_unique_ID))
  # remove columns with just the well position
  formatted_df <- Filter(function(x)(length(unique(x))>1), formatted_df)
  # rename column with just the delimiter
  colnames(formatted_df) <- sub("\\..*", "", colnames(formatted_df))
  # ensure the well numbers and cycle are captured as the first row in the data frame
  formatted_df <- rbind(colnames(formatted_df), formatted_df)
  # rownames should match the appropriate data (built_unique_ID then cycle numbers)
  rownames(formatted_df) <- c("built_unique_ID", paste0("Cycle", 1:(nrow(formatted_df)-1)))
  # transpose the df such that each well is a row
  formatted_df <- as.data.frame(t(formatted_df))
  return(formatted_df)
}

#################### MAIN FUNCTION ####################

#process_Multiplexed_RDML <- function (rdml_file) {
process_Multiplexed_RDML <- function (fdata) {

print("Beginning of the data_processing_helpers process_Multiplexed_RDML function")

  #Build a unique ID to mirror the metadata file unique ID
  fdata <- fdata[, c( "exp.id", "run.id", "react.id","fluor")]
  built_unique_ID<-paste0(fdata$exp.id,"_",fdata$run.id,"_", fdata$react.id)
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
  rownames(formatted_df) <- c("built_unique_ID", paste0("Cycle", 1:(nrow(formatted_df)-1)))
  # transpose the df such that each well is a row
  formatted_df <- as.data.frame(t(formatted_df))

print("End of the process_Multiplexed_RDML function")

  return (formatted_df)
}

############### Format the Standard Curve Metadata ##################

#format_std_curve_metadata <- function (standardCurve_metadata) {
#  standardCurve_Table <- read_excel(standardCurve_metadata, sheet = 5)
#  standardCurve_Table <- as.data.frame(standardCurve_Table[, c("SCresultID",
#                                                               "runID",
#                                                               "pcrChemistryID",
#                                                               "standardCurveID",
#                                                               "mdmaprThres",
#                                                               "sampleName",
#                                                               "copyNumber",
#                                                               "control",
#                                                               "standardConc",
#                                                               "userProvidedThresholdValue",
#                                                               "userProvidedCqValue",
#                                                               "runRecordedBy",
#                                                               "runDate",
#                                                               "runTime",
#                                                               "runPlatform",
#                                                               "machineID",
#                                                               "reactionConditions",
#                                                               "reactionVolume",
#                                                               "templateAmount",
#                                                               "forwardPrimerBatch",
#                                                               "reversePrimerBatch",
#                                                               "dNTPConcentration",
#                                                               "primerConcentration",
#                                                               "probeConcentration",
#                                                               "Mg2+Concentration",
#                                                               "polymeraseBatch",
#                                                               "polymeraseConcentrations",
#                                                               "thermocyclerParameters",
#                                                               "pcrDataNotes",
#                                                               "assayID",
#                                                               "standardCurveName",
#                                                               "SCdate",
#                                                               "SCrecordedBy",
#                                                               "SCdataNotes",
#                                                               "LOD",
#                                                               "LOQ")])
#  return (standardCurve_Table)
#}

############### Removing the control records##################

#remove_null_records <- function(meta_data, fluor_file_list){
#  # This function will change the list and metadata files in place
#  meta_data_name = deparse(substitute(meta_data))
#  fluor_file_list_name = deparse(substitute(fluor_file_list))
#  if(length(which(grepl("null", tolower(meta_data$sampleName))))!=0){
#    # get the position of the records that have null in the sample names
#    control_well_locations <- as.character(meta_data$mdmaprThres[which(tolower(meta_data$sampleName)=="null")])
#    # remove these records from the fluorescence files (if present)
#    fluor_file_list <- lapply(fluor_file_list, function(x) x[-c(which(as.character(x$mdmaprThres) %in%
#                                                                        control_well_locations)),])
#    # remove these records from the metadata
#    meta_data <- meta_data[-c(which(grepl("null", tolower(meta_data$sampleName)))), ]}
#  # create a list to store these elements (they will be unlisted and brought back into the global environment)
#  cleaned_data <- list(fluor_file_list, meta_data)
#  return(cleaned_data)
#}


#remove_null_records_test <- function(formatted_metadata, raw_multiplex_data_list){
#  # This function will change the list and metadata files in place
#  meta_data_name = deparse(substitute(formatted_metadata))
#  fluor_file_list_name = deparse(substitute(raw_multiplex_data_list))
#  if(length(which(grepl("null", tolower(formatted_metadata$sampleName))))!=0){
#    # get the position of the records that have null in the sample names
#    control_well_locations <- as.character(formatted_metadata$mdmaprThres[which(tolower(formatted_metadata$sampleName)=="null")])
#    # remove these records from the fluorescence files (if present)
#    raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x) x[-c(which(as.character(x$mdmaprThres) %in%
#                                                                                        control_well_locations)),])
#    # remove these records from the metadata
#    # create a list to store these elements (they will be unlisted and brought back into the global environment)
#    cleaned_data <- list(raw_multiplex_data_list, formatted_metadata)
#  }
#  else{
#    cleaned_data <- list(raw_multiplex_data_list, formatted_metadata)
#  }
#  return(cleaned_data)
#}


########## Second Derivative Threshold Calculation #####################

calculate_second_deriv_threshold <- function(fluorescence_values_df){

  fluorescence_values <- as.data.frame(t(fluorescence_values_df))
  # now each column is a sample and each row is a cycle

  # ensure all values are NUMERIC
  fluorescence_values[] <- lapply(fluorescence_values, as.numeric)

  #New table with threshold data
  thresholdData <- as.data.frame(matrix(nrow = ncol(fluorescence_values) , ncol = 1))
  colnames(thresholdData) <- c("mdmaprThres")
  rownames(thresholdData) <- colnames(fluorescence_values)

  # for each sample (column), find the lowest absorbance - this will be our minimum
  for (runSample in 1:ncol(fluorescence_values)) {
    #Get the total number of thremocycler cycles for this sample
    number_of_cycles <- length(fluorescence_values [,runSample])

    #Set the initial absorbance cycle to the cycle with the minimum absorbance value
    place_holder_cycle = which.min(fluorescence_values[, runSample])

    # loop through absorbance values

    # check # 1: do the values
    #Step through the data points for the cycles from the minimum value to the end of the cycles to see if the data is increasing within a certain percentage as compared to the total absorbance range for the sample
    exp_phase_subset <- c()

    for (cycle_index in place_holder_cycle:(length(fluorescence_values[, runSample])-1)) {
      # calculate the absolute value difference between that cycle and the following cycle
      absolute <- abs((fluorescence_values[cycle_index, runSample]) - (fluorescence_values[cycle_index+1, runSample]))
      maximum <- max((fluorescence_values[cycle_index, runSample]),(fluorescence_values[cycle_index+1, runSample]))
      if((absolute/maximum) >= 0.02) {
        # this vector will contain the cycle number index that meet the defined criteria
        exp_phase_subset <- c(exp_phase_subset, cycle_index)

      }
    }

    #Jan 2 - changed the systemCalculatedThresholdValue to the variable mdmaprThres reflective of the single flat file
    if (is.null(exp_phase_subset)|length(exp_phase_subset)<2){
      thresholdData$mdmaprThres[runSample] <- "Unable to Determine Threshold"
    }

    else{
      # update the place holder cycle to the minimum of this subset
      place_holder_cycle <- min(exp_phase_subset)
      # calculate the threshold

      #Subset dataframe to plot it
      data_to_plot <- as.data.frame(as.numeric(t(fluorescence_values[exp_phase_subset, runSample])))

      data_to_plot <- cbind(as.data.frame(c(exp_phase_subset)), data_to_plot)

      # This is the section where I get the second derivative of the curve and determine the value at which we will set the threshold

      deriv <- function(x, y)
        diff(y) / diff(x)

      middle_pts <- function(x)
        x[-1] - diff(x) / 2

      second_d <- deriv(middle_pts(data_to_plot[, 1]), deriv(data_to_plot[, 1], data_to_plot[, 2]))


      #Getting the max value of the second derivative data set. This will represent the points between the values on the curve between the end of the noise data and the end of the data set.
      #So we need to add the max value index to the placeholder and then add one to round up to the next value to get the index (or cycle number) on the original data set.
      max_deriv_value_index <- which.max(second_d)

      #The threshold value at the cycle determined by the double derivitave is...
      #we need to add one
      threshold_value <- fluorescence_values[(place_holder_cycle + max_deriv_value_index),runSample]


      #Add threshold value to dataframe
      thresholdData$mdmaprThres[runSample] <- threshold_value
    }
  }
  thresholdData$built_unique_ID <- rownames(thresholdData)
  fluorescence_values_df <- as.data.frame(fluorescence_values_df)
  fluorescence_values_df$built_unique_ID <- rownames(as.data.frame(fluorescence_values_df))
  fluorescence_values_df <- merge(fluorescence_values_df, thresholdData, by="built_unique_ID")
  rownames(fluorescence_values_df) <- fluorescence_values_df$built_unique_ID

  return(fluorescence_values_df)
}

############### Calculating the Cq Value ##################

add_Cq <- function(fluorescence_df, threshold_val_column, Cq_val_col){

print("data_processing_helpers - add_Cq function - BEGIN ")

  # subset the data with only the fluorescence values
  flu_data_subset <- fluorescence_df[,grep("Cycle", names(fluorescence_df), value = TRUE)]

  for (i in 1:nrow(fluorescence_df)){

    if (is.na(fluorescence_df[i,threshold_val_column])){
      fluorescence_df[i,Cq_val_col] <- NA
    }else if(fluorescence_df[i,threshold_val_column]== "Unable to Determine Threshold"){
      #If the threshold value was unable to be determined then set the Cq to the
      # max cycle number for the experiment
      fluorescence_df[i,Cq_val_col] <- ncol(flu_data_subset[i,])
    }else{
      fluorescence_df[i,Cq_val_col] <- as.numeric(as.character(
        th.cyc(seq(1:ncol(flu_data_subset)),
               as.numeric(flu_data_subset[i, ]),
               r = round(as.numeric(fluorescence_df[i,threshold_val_column]), 3),
               linear = TRUE)[1]))
    }
  }

print("data_processing_helpers - add_Cq function - END ")

  return(fluorescence_df)
}

############### Calculating the Copy Number ##################

calculate_copy_number <- function(standard_curve_flu, experimental_flu){
  regression_line <- lm(log(standardConc) ~ systemCalculatedCqValue, standard_curve_flu, na.action=na.exclude)
  testing_cq <- as.data.frame(experimental_flu$systemCalculatedCqValue)
  colnames(testing_cq) <- "systemCalculatedCqValue"
  predictions <- as.data.frame(predict(regression_line, testing_cq))
  colnames(predictions) <- "logDNACopyNumber"
  predictions$mdmaprThres <- experimental_flu$mdmaprThres
  predictions$rSquared <- rep(as.numeric(format(summary(regression_line)$r.squared, digits=3)),length(predictions$mdmaprThres))
  return(predictions)
}

############### Functions for processing the standard curve flu ##################

# For MIC
#process_MIC_raw_data <- function(raw_fluorescence){
#  raw_fluorescence <- as.data.frame(t(raw_fluorescence[, 2:ncol(raw_fluorescence)]))
#  # add consistent column names
#  #Changing column names to consistent with naming convention in database
#  total_runs <- ncol(raw_fluorescence)
#  colnames(raw_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))
#  # add a column for merging with metadata
#  raw_fluorescence$wellLocation <- as.numeric(gsub("[^0-9]", "", rownames(raw_fluorescence)))
#  # move wellLocation to the front
#  raw_fluorescence <- as.data.frame(raw_fluorescence[,c(ncol(raw_fluorescence),1:ncol(raw_fluorescence)-1)])
#  print(paste0("number of wells: ", nrow(raw_fluorescence), ", number of cycles: ", ncol(raw_fluorescence)-1))
#  return(raw_fluorescence)
#}

# For Biomeme

#process_biomeme_raw_data <- function(raw_fluorescence){
#  #Creating dataframe with fluorescence values from biomem raw qPCR file
#  end_row <-(which(grepl('Raw Fluorescence', raw_fluorescence$Run.Name))) - 2
#  raw_fluorescence <- raw_fluorescence[11:end_row, 2:41]
#  #Changing column names to consistent with naming convention in database
#  total_runs <- ncol(raw_fluorescence)
#  colnames(raw_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))
#  #change all values to numeric
#  cols = c(1:total_runs)
#  raw_fluorescence[, cols] <- apply(raw_fluorescence[, cols], 2, function(x)  as.numeric(as.character(x)))
#  # add wellLocation (key for merging)
#  raw_fluorescence$wellLocation <- fluorescence_file[11:end_row, 1]
#  raw_fluorescence <- as.data.frame(raw_fluorescence[,c(ncol(raw_fluorescence),1:ncol(raw_fluorescence)-1)])
#  print(paste0("number of wells: ", nrow(raw_fluorescence), ", number of cycles: ", ncol(raw_fluorescence)-1))
#  return (raw_fluorescence)
#}

############### Functions for processing merged file ##################

#merged_file_processing <- function(merged_file, dataset_name){
#merged_file_processing <- function(merged_file){
  # handling extreme cq values

#  merged_file$systemCalculatedCqValue <- as.numeric(merged_file$systemCalculatedCqValue)
#  merged_file$systemCalculatedCqValue[merged_file$systemCalculatedCqValue < 0] <- 40
#  merged_file$systemCalculatedCqValue[merged_file$systemCalculatedCqValue > 40] <- 40

#  merged_file$CqIntensitySystemCalculated <- cut(merged_file$systemCalculatedCqValue,
#                                                 breaks = c(0, 10, 20, 30, 40, 1000),
#                                                 right = FALSE,
#                                                 labels = c("0 < Very strong < 10",
#                                                            "10 <= Strong < 20",
#                                                            "20 <= Moderate < 30",
#                                                            "30 <= Weak < 40",
#                                                            "None > 40"))
#  #Changing mdmaprThres column class to character
#  merged_file$mdmaprThres <- as.character(merged_file$mdmaprThres)

  # ensuring date values have a date format
#  merged_file$projectCreationDate <- as.Date(as.character(merged_file$projectCreationDate), "%Y-%m-%d")
#  merged_file$collectionDate <- as.Date(as.character(merged_file$collectionDate ), "%Y-%m-%d")
#  merged_file$extractionDate <- as.Date(as.character(merged_file$extractionDate), "%Y-%m-%d")
#  merged_file$runDate <- as.Date(as.character(merged_file$runDate), "%Y-%m-%d")
#  merged_file$assayDate <- as.Date(as.character(merged_file$assayDate), "%Y-%m-%d")

  # adding dataset name
#  merged_file$dataset_name <- dataset_name

  # remove one of the userprovided threshold columns and rename the other
#  merged_file$userProvidedThresholdValue.x <- NULL
#  colnames(merged_file)[colnames(merged_file) == 'userProvidedThresholdValue.y'] <- 'userProvidedThresholdValue'

  # adding
#  merged_file$copyNumber <- merged_file$logDNACopyNumber
#  merged_file$logDNACopyNumber <- NULL
  #merged_file$copyNumber <- ifelse(merged_file$copyNumber<=0, 0, merged_file$copyNumber)

  # create a dataframe that will add the cycle number until 70

#  number_of_cycles = length(grep(x = colnames(merged_file), pattern = "Cycle_Number"))

#  filler_df <- setNames(data.frame(matrix(nrow=nrow(merged_file), ncol=70-number_of_cycles)), c(paste0("Cycle_Number", (number_of_cycles+1):70)))

  # we can cbind this to our dataframe.
#  merged_file <- cbind(merged_file, filler_df)

  # now we'll set the order of the values
#  merged_file <- merged_file[ , c("dataset_name", "resultID", "runID", "assayID", "pcrChemistryID", "extractID", "mdmaprThres", "sampleName", "copyNumber", "control", "userProvidedThresholdValue", "CqvaluewithUserThreshold", "systemCalculatedThresholdValue","systemCalculatedCqValue", "userProvidedCqValue","CqIntensitySystemCalculated", "rSquared", "Cycle_Number1",	"Cycle_Number2", "Cycle_Number3",	"Cycle_Number4",	"Cycle_Number5", "Cycle_Number6",	"Cycle_Number7",	"Cycle_Number8", "Cycle_Number9",	"Cycle_Number10",	"Cycle_Number11","Cycle_Number12",	"Cycle_Number13",	"Cycle_Number14","Cycle_Number15",	"Cycle_Number16",	"Cycle_Number17","Cycle_Number18",	"Cycle_Number19",	"Cycle_Number20","Cycle_Number21",	"Cycle_Number22",	"Cycle_Number23","Cycle_Number24",	"Cycle_Number25",	"Cycle_Number26","Cycle_Number27",	"Cycle_Number28",	"Cycle_Number29", "Cycle_Number30",	"Cycle_Number31",	"Cycle_Number32","Cycle_Number33",	"Cycle_Number34",	"Cycle_Number35", "Cycle_Number36", "Cycle_Number37","Cycle_Number38","Cycle_Number39","Cycle_Number40","Cycle_Number41", "Cycle_Number42", "Cycle_Number43", "Cycle_Number44", "Cycle_Number45", "Cycle_Number46", "Cycle_Number47", "Cycle_Number48", "Cycle_Number49", "Cycle_Number50", "Cycle_Number51", "Cycle_Number52", "Cycle_Number53", "Cycle_Number54", "Cycle_Number55", "Cycle_Number56", "Cycle_Number57", "Cycle_Number58", "Cycle_Number59", "Cycle_Number60", "Cycle_Number61", "Cycle_Number62", "Cycle_Number63", "Cycle_Number64", "Cycle_Number65", "Cycle_Number66", "Cycle_Number67", "Cycle_Number68", "Cycle_Number69", "Cycle_Number70", "runRecordedBy","runDate", "runTime", "runPlatform", "machineID", "reactionConditions", "reactionVolume", "templateAmount", "forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration", "probeConcentration", "Mg2+Concentration", "polymeraseBatch", "polymeraseConcentrations", "thermocyclerParameters", "pcrDataNotes", "taxonID", "establishmentMeans", "assayName", "assayOwnership", "assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol", "dilutions", "replicates", "primerR", "primerF", "probe", "ampliconLength (bp)", "probeFluorescentTag", "dye(s)", "quencher", "probeModification", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "species", "vernacularName", "organismScope", "replicateID", "extractName", "analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes", "tubePlateID", "frozen", "fixed", "dnaStorageLocation", "extractMethodOfStorage", "dnaVolume", "quantificationMethod", "concentration(ng/ul)", "stationID", "collectorName", "replicateName", "collectionDate", "collectionTime", "storageID", "DateOfStorage", "methodOfStorage", "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation", "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "flowRate(m/s)", "filterType", "filtrationDuration(mins)", "volumeFiltered", "processLocation", "replicationNumber", "riparianVegetationPercentageCover", "dissolvedOxygen(mg/L)", "waterTemperature(C)", "pH", "TSS(mg/L)", "EC(uS/cm)", "turbidity(NTU)", "discharge", "tide", "chlorophyl", "salinity(ppt)", "contaminants(ng/g)", "traceMetals(mg/kg)", "organicContent(%)", "microbialActivity", "grainSize", "replicateDataNotes", "siteID", "stationName", "decimalLongitude", "decimalLatitude", "geographicRegionID", "locality", "estimatedPerimeter", "estimatedSurfaceArea(m2)", "siteType", "siteLength(m2)", "projectID", "continent", "country", "stateProvince", "municipality", "projectCreationDate","projectName", "projectRecordedBy", "projectOwner", "projectContactEmail", "projectDescription", "InstitutionID", "projectDataNotes", "standardCurveID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes")]
#  merged_file <- merged_file[ , c("resultID", "runID", "assayID", "pcrChemistryID", "extractID", "mdmaprThres", "sampleName", "copyNumber", "control", "userProvidedThresholdValue", "CqvaluewithUserThreshold", "systemCalculatedThresholdValue","systemCalculatedCqValue", "userProvidedCqValue","CqIntensitySystemCalculated", "rSquared", "Cycle_Number1",	"Cycle_Number2", "Cycle_Number3",	"Cycle_Number4",	"Cycle_Number5", "Cycle_Number6",	"Cycle_Number7",	"Cycle_Number8", "Cycle_Number9",	"Cycle_Number10",	"Cycle_Number11","Cycle_Number12",	"Cycle_Number13",	"Cycle_Number14","Cycle_Number15",	"Cycle_Number16",	"Cycle_Number17","Cycle_Number18",	"Cycle_Number19",	"Cycle_Number20","Cycle_Number21",	"Cycle_Number22",	"Cycle_Number23","Cycle_Number24",	"Cycle_Number25",	"Cycle_Number26","Cycle_Number27",	"Cycle_Number28",	"Cycle_Number29", "Cycle_Number30",	"Cycle_Number31",	"Cycle_Number32","Cycle_Number33",	"Cycle_Number34",	"Cycle_Number35", "Cycle_Number36", "Cycle_Number37","Cycle_Number38","Cycle_Number39","Cycle_Number40","Cycle_Number41", "Cycle_Number42", "Cycle_Number43", "Cycle_Number44", "Cycle_Number45", "Cycle_Number46", "Cycle_Number47", "Cycle_Number48", "Cycle_Number49", "Cycle_Number50", "Cycle_Number51", "Cycle_Number52", "Cycle_Number53", "Cycle_Number54", "Cycle_Number55", "Cycle_Number56", "Cycle_Number57", "Cycle_Number58", "Cycle_Number59", "Cycle_Number60", "Cycle_Number61", "Cycle_Number62", "Cycle_Number63", "Cycle_Number64", "Cycle_Number65", "Cycle_Number66", "Cycle_Number67", "Cycle_Number68", "Cycle_Number69", "Cycle_Number70", "runRecordedBy","runDate", "runTime", "runPlatform", "machineID", "reactionConditions", "reactionVolume", "templateAmount", "forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration", "probeConcentration", "Mg2+Concentration", "polymeraseBatch", "polymeraseConcentrations", "thermocyclerParameters", "pcrDataNotes", "taxonID", "establishmentMeans", "assayName", "assayOwnership", "assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol", "dilutions", "replicates", "primerR", "primerF", "probe", "ampliconLength (bp)", "probeFluorescentTag", "dye(s)", "quencher", "probeModification", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "species", "vernacularName", "organismScope", "replicateID", "extractName", "analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes", "tubePlateID", "frozen", "fixed", "dnaStorageLocation", "extractMethodOfStorage", "dnaVolume", "quantificationMethod", "concentration(ng/ul)", "stationID", "collectorName", "replicateName", "collectionDate", "collectionTime", "storageID", "DateOfStorage", "methodOfStorage", "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation", "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "flowRate(m/s)", "filterType", "filtrationDuration(mins)", "volumeFiltered", "processLocation", "replicationNumber", "riparianVegetationPercentageCover", "dissolvedOxygen(mg/L)", "waterTemperature(C)", "pH", "TSS(mg/L)", "EC(uS/cm)", "turbidity(NTU)", "discharge", "tide", "chlorophyl", "salinity(ppt)", "contaminants(ng/g)", "traceMetals(mg/kg)", "organicContent(%)", "microbialActivity", "grainSize", "replicateDataNotes", "siteID", "stationName", "decimalLongitude", "decimalLatitude", "geographicRegionID", "locality", "estimatedPerimeter", "estimatedSurfaceArea(m2)", "siteType", "siteLength(m2)", "projectID", "continent", "country", "stateProvince", "municipality", "projectCreationDate","projectName", "projectRecordedBy", "projectOwner", "projectContactEmail", "projectDescription", "InstitutionID", "projectDataNotes", "standardCurveID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes")]
#  return(merged_file)

#}




