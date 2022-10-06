
output_csv <- function(data,
                       filepath,
                       overwrite = TRUE) {

  if(is.null(data)){
      return(NULL)
      message("No data found - skipping write to csv")
  }
      
    
  target_dir = dirname(filepath)
  # Check if output dir exists and try to create if not
  dir_exists = ifelse(!dir.exists(target_dir),
                      dir.create(target_dir, recursive = TRUE),
                      TRUE)

  message(paste0("Trying to write to: ", filepath))

    if(!dir_exists){
    stop(sprintf("Directory %s does not exist and could not be created",
                 target_dir))
  }
  
  message(paste0("Successfully found directory: ", target_dir))
  write_csv(data, filepath)
  
  return(filepath)
}
