download_single_table <- function(target_db, 
                                  table,
                                  device_ids, 
                                  filepath,
                                  subject_id,
                                  max_timestamp,
                                  overwrite = FALSE) {
  
  if(file.exists(filepath) && overwrite == FALSE){
    print(paste0("File already exists for ", table,
                 " for subject_id = ", subject_id,
                 " at location: ", filepath))
    return(filepath)
  }
  
  subject_data = NULL
  for (device_id in device_ids) {
    print(paste0("Querying table ", table,
                 " for subject_id = ", subject_id,
                 " and device_id = ", device_id))
    
    query <- paste0(
      "select *",
      " from ",
      table,
      " where device_id = '",
      device_id,
      "' AND timestamp <= ",
      max_timestamp,
      " order by timestamp;"
    )
    device_data <- run_query_on_db(target_db, query)
    
    if(is.null(subject_data)){
      subject_data <- device_data
    }
    else {
      subject_data = rbind(subject_data, device_data)
    }
  }
  
  # Remove duplicates and sort combined device data
  subject_data <- subject_data %>%
    arrange(timestamp) %>%
    distinct()
  
  # Write file to output_file file path
  saveRDS(subject_data, filepath)
  return(filepath)
}
download_all_tables <- function(target_db, 
                                table_names,
                                device_ids, 
                                subject_id, 
                                download_dir,
                                max_timestamp,
                                overwrite) {
  
  print(paste0("Beginning download for subject_id = ", subject_id))

  target_dir = file.path(download_dir)
  # Check if output dir exists and try to create if not
  dir_exists = ifelse(!dir.exists(target_dir),
                      dir.create(target_dir, recursive = TRUE),
                      TRUE)
  
  if(!dir_exists){
    stop(sprintf("Directory %s does not exist and could not be created",
                 target_dir))
  }
  filepaths = as.character(vector())
  for(table in table_names){
    target_filepath = file.path(target_dir,paste(subject_id, table, "rds", sep ="."))
    download_single_table(target_db = target_db, # Name of DB for accessing env credentials
                          table = table, # name of table in AWARE database
                          device_ids = device_ids, # map from each subject_id to list of device_ids
                          filepath = target_filepath,
                          max_timestamp = max_timestamp,
                          subject_id = subject_id,
                          overwrite = overwrite)
    filepaths = append(filepaths, target_filepath)
  }
  
  print(filepaths)
  return(filepaths)
}

download_all_cat_results <- function(target_db, 
                                     filepath,
                                     max_datetime,
                                     overwrite = FALSE) {
  
  if(file.exists(filepath) && overwrite == FALSE){
    print(paste0("CAT file already exists at location: ", filepath))
    return(filepath)
  }
  
  query <- paste0("SELECT
                   study_subjects.subject_id,
                   study_subjects.study_name,
                   study_subjects.study_subject_id,
                   study_subjects.study_identifier,
                   study_subjects.exclude,
                   cat_interviews.cat_subject_id,
                   cat_interviews.start_time,
                   cat_interviews.end_time,
                   cat_tests.*
                   FROM
                   cat_tests
                   LEFT JOIN
                   cat_interviews ON cat_tests.interview_id = cat_interviews.interview_id
                   LEFT JOIN
                   study_subjects ON cat_interviews.cat_subject_id = study_subjects.cat_subject_id
                   WHERE study_subjects.exclude = 0 AND
                   cat_tests.created_at <= '", max_datetime, "' ")
  print(query)
  cat_data <- run_query_on_db(target_db, query)
  
  target_dir = dirname(filepath)
  # Check if output dir exists and try to create if not
  dir_exists = ifelse(!dir.exists(target_dir),
                      dir.create(target_dir, recursive = TRUE),
                      TRUE)
  
  if(!dir_exists){
    stop(sprintf("Directory %s does not exist and could not be created",
                 target_dir))
  }
  
  write_csv(cat_data, filepath)
  return(filepath)
}
