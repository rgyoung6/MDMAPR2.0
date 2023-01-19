#################### File Validation Functions for MDMAPR ##########

#Function that is opposite on 'is in' function
'%ni%' <- Negate('%in%')


########### These validations happen as the user uploads the file#####
#Function to give pop-up validation messages for uploaded fluorescence file., is it the correct file type?
#fluorescence_file_validation_msgs <- function(flur_file) {

#  if (file_ext(flur_file$datapath) %ni% c("rdml"))
#  {shinyjs::alert("ERROR: Fluorescence file is not RDML.")}

#}

#Function to give pop-up validation messages for uploaded metadata file., is format of the metadata good (only applies to the sheets related to the fluorescence file)
#metadata_file_validation_msgs <- function(meta_file){

#  if (file_ext(meta_file$datapath) %ni% c("tsv"))
#  {shinyjs::alert("ERROR: Metadata file is not an accepted file type.")}


  #NOTE the following format_qPCR_metadata function no longer exists. I should create
  #a new one to verify the bare minimum cloumns are present


  #Check is metadata parsing function works on file
#  else if (is.error(format_qPCR_metadata(meta_file$datapath)) == TRUE)
#  {shinyjs::alert("ERROR: Metadata file is missing one or more necessary columns.")}

#}
