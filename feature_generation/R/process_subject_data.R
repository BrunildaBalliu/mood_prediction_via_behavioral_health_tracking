preprocess_subject_data <- function(subject_raw_data) {
  preprocessed_data = subject_raw_data
  for(table in names(preprocessed_data)){
    if(!is.null(preprocessed_data[[table]])){
      if(table == "cat"){
        preprocessed_data[[table]] <- subject_raw_data[[table]]
      }
      else{
        preprocessed_data[[table]] <- expand_timestamp_metrics(preprocessed_data[[table]])
      }
    }
  }
  # Filter GPS data where points do not have a valid location 
  if(!is.null(preprocessed_data[["locations"]])){
    preprocessed_data[["locations"]] <- preprocessed_data[["locations"]] %>%
      dplyr::filter(double_longitude != 0 & double_latitude !=0)
  }
  
  
  return(preprocessed_data)
}
