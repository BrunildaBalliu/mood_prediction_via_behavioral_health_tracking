read_raw_data <- function(filepath,
                          type = NULL){
  
  col_spec = cols(.default = col_character(),
                  X1 = col_double(),
                  `_id` = col_double(),
                  accuracy = col_double(),
                  activities = col_character(),
                  activity_name = col_character(),
                  activity_type = col_character(),
                  application_name = col_character(),
                  automotive = col_double(),
                  call_duration = col_double(),
                  call_type = col_double(),
                  cat_subject_id = col_character(),
                  category = col_character(),
                  confidence = col_character(),
                  confidence = col_double(),
                  created_at = col_datetime(format = ""),
                  cycling = col_double(),
                  device_id = col_character(),
                  diagnosis = col_character(),
                  double_altitude = col_double(),
                  double_bearing = col_double(),
                  double_latitude = col_double(),
                  double_longitude = col_double(),
                  double_speed = col_double(),
                  end_time = col_datetime(format = ""),
                  is_system_app = col_character(),
                  label = col_character(),
                  message_type = col_character(),
                  package_name = col_character(),
                  provider = col_character(),
                  running = col_double(),
                  screen_status = col_double(),
                  start_time = col_datetime(format = ""),
                  stationary = col_double(),
                  study_identifier = col_character(),
                  study_name = col_character(),
                  study_subject_id = col_character(),
                  subject_id = col_character(),
                  timestamp = col_double(),
                  trace = col_character(),
                  type = col_character(),
                  unknown = col_double(),
                  updated_at = col_datetime(format = ""),
                  walking = col_double()
  )
  
  print(str_glue("Reading file:  {filepath}"))
  
  headers = read_csv(filepath, n_max = 0, col_types = cols(.default = col_character()))
  header_names = names(headers)
  col_spec_filtered = col_spec
  col_spec_filtered$cols <- col_spec$cols[names(col_spec$cols) %in% header_names]
  
  file_data = read_csv(filepath, col_types = col_spec_filtered)
  
  return(file_data)
}


load_raw_data <- function(dir,
                          sub_dir,
                          aware_tables,
                          cat_table) {
  
  col_spec = cols(.default = col_character(),
                  X1 = col_double(),
                  `_id` = col_double(),
                  accuracy = col_double(),
                  activities = col_character(),
                  activity_name = col_character(),
                  activity_type = col_character(),
                  application_name = col_character(),
                  automotive = col_double(),
                  call_duration = col_double(),
                  call_type = col_double(),
                  cat_subject_id = col_character(),
                  category = col_character(),
                  confidence = col_character(),
                  confidence = col_double(),
                  created_at = col_datetime(format = ""),
                  cycling = col_double(),
                  device_id = col_character(),
                  diagnosis = col_character(),
                  double_altitude = col_double(),
                  double_bearing = col_double(),
                  double_latitude = col_double(),
                  double_longitude = col_double(),
                  double_speed = col_double(),
                  end_time = col_datetime(format = ""),
                  is_system_app = col_character(),
                  label = col_character(),
                  message_type = col_character(),
                  package_name = col_character(),
                  provider = col_character(),
                  running = col_double(),
                  screen_status = col_double(),
                  start_time = col_datetime(format = ""),
                  stationary = col_double(),
                  study_identifier = col_character(),
                  study_name = col_character(),
                  study_subject_id = col_character(),
                  subject_id = col_character(),
                  timestamp = col_double(),
                  trace = col_character(),
                  type = col_character(),
                  unknown = col_double(),
                  updated_at = col_datetime(format = ""),
                  walking = col_double()
  )
  
  
  
  all_tables = c(aware_tables,cat_table)

  subject_data = list()
  for(table in all_tables){
    filepath = paste0(file.path(dir,sub_dir,table),".csv")
    print(paste0("Reading file: ",filepath))
    
    headers = read_csv(filepath, n_max = 0, col_types = cols(.default = col_character()))
    header_names = names(headers)
    col_spec_filtered = col_spec
    col_spec_filtered$cols <- col_spec$cols[names(col_spec$cols) %in% header_names]
    
    file_data = read_csv(filepath, col_types = col_spec_filtered)
    
    if(nrow(file_data)>0){
      subject_data[[table]] <- file_data
    }
    else{
      subject_data[[table]] <- NULL
    }
  }
  
  return(subject_data)
}
