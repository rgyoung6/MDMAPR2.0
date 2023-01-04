#################### File Validation Functions for MDMAPR ##########

#Function that is opposite on 'is in' function
'%ni%' <- Negate('%in%')


########### These validations happen as the user uploads the file#####
#Function to give pop-up validation messages for uploaded fluorescence file., is it the correct file type?
fluorescence_file_validation_msgs <- function(flur_file) {

  if (file_ext(flur_file$datapath) %ni% c("rdml"))
  {shinyjs::alert("ERROR: Fluorescence file is not RDML.")}

}

#Function to give pop-up validation messages for uploaded metadata file., is format of the metadata good (only applies to the sheets related to the fluorescence file)
metadata_file_validation_msgs <- function(meta_file){

print("file_validation_helpers - metadata_file_validation_msgs - 1")

  if (file_ext(meta_file$datapath) %ni% c("tsv"))
  {shinyjs::alert("ERROR: Metadata file is not an accepted file type.")}

  #Check is metadata parsing function works on file
  else if (is.error(format_qPCR_metadata(meta_file$datapath)) == TRUE)
  {shinyjs::alert("ERROR: Metadata file is missing one or more necessary columns.")}

}

#Function runs validation tests on user uploaded files.
user_uploaded_file_validate <- function(fluor_file, metadata_file){

  # return an error if necessary files are missing
  if (is.null(fluor_file) | is.null(metadata_file)){
    print("null files")
    return(FALSE)

  # return an error if the experimental file is not rdml
  }else if (file_ext(fluor_file$datapath) %ni% c("rdml")){
    print("rdml type not passed")
    return(FALSE)

  # return an error if the experimental metadata file is not tsv
  }else if (file_ext(metadata_file$datapath) %ni% c("tsv")){
    print("rdml type not passed")
    return(FALSE)

  }else{
    return(TRUE)
  }#End of if/else statements

}






