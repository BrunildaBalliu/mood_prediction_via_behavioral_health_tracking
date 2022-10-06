preprocess_subject_data_2 <- function (input_dir,
                                       output_dir,
                                       subject_id,
                                       tables = NULL,
                                       overwrite = TRUE) {
  
  wrap_preprocess <- function(row){
    result = preprocess_subject_file(input_filepath = row$input_filepath,
                                     output_filepath = row$output_filepath,
                                     table_type = row$table_type,
                                     subject_id = row$subject_id)
    
    return(data.frame(result))    
  }
  
  input_filepath = list.files(input_dir, pattern = paste0("^",subject_id,"[.].*rds$"), full.names = TRUE)
  files_tbl = tibble(input_filepath) %>%
    mutate(input_filename = basename(input_filepath),
           input_directory = dirname(input_filepath),
           input_filename_san_ext = file_path_sans_ext(input_filename)) %>%
    mutate(tmp = input_filename) %>%
    separate(tmp, c("subject_id", "table_type", "extension"), sep = "[.]") %>%
    mutate(output_filepath = file.path(output_dir, paste0(input_filename_san_ext, ".rds")))
  

  
  
  result = files_tbl %>% rowwise() %>% do(wrap_preprocess(.))
  
  return(files_tbl$output_filepath)
}

preprocess_subject_file <- function(input_filepath,
                                    output_filepath,
                                    table_type,
                                    subject_id) {
  
  if(file_ext(input_filepath) == "csv"){
    data = read_raw_data(input_filepath)
  } else if(file_ext(input_filepath) == "rds"){
    data = readRDS(input_filepath)
  } else{
    warning(str_glue("Unable to read file {input_filepath} due to unrecognized extension"))
  }

  if("_id" %in% colnames(data))
    data %<>% select(-"_id") 
  
  data %<>% distinct() %>%
    mutate(subject_id = subject_id) %>% 
    relocate(subject_id)
  

  
  ## cat
  if(table_type == "cat"){
    data %<>% arrange(start_time)
    
    ## locations  
  } else if(table_type == "locations"){
    data %<>% dplyr::filter(double_longitude != 0 & double_latitude !=0)
    data %<>% distinct(timestamp, double_latitude, double_longitude, .keep_all = TRUE)
    data %<>% arrange(timestamp)
    data %<>% expand_timestamp_metrics()
    
    ## applications_foreground
  } else if(table_type == "applications_foreground"){
    data %<>% arrange(timestamp)
    data %<>% expand_timestamp_metrics()
    
    ## plugin_google_activity_recognition
  } else if(table_type == "plugin_google_activity_recognition"){
    data %<>% arrange(timestamp)
    data %<>% expand_timestamp_metrics()
    
    ## plugin_ios_activity_recognition
  } else if(table_type == "plugin_ios_activity_recognition"){
    data %<>% arrange(timestamp)
    data %<>% expand_timestamp_metrics()
    
    ## calls
  } else if(table_type == "calls"){
    data %<>% arrange(timestamp)
    data %<>% expand_timestamp_metrics()
    
    ## screen
  } else if(table_type == "screen"){
    data %<>% arrange(timestamp)
    data %<>% distinct(screen_status, timestamp, .keep_all = TRUE)
    data = expand_timestamp_metrics(data)
    
    #TODO look at transition states between off/on in android device (iOS only uses locked/unlocked)
    # screen_key = list("0"="off", "1"="on", "2"="locked", "3"="unlocked")
    screen_key = list("0"="off", "1"="on", "2"="off", "3"="on")
    screen_translated = unlist(screen_key[as.character(data$screen_status)])
    if(nrow(data)>0){
      screen_translated = unlist(screen_key[as.character(data$screen_status)])
    }else{
      screen_translated = NA  
    }
    data %<>%
      mutate(status_new = screen_translated,
             status_prev = lag(status_new),
             status_changed = (status_new != status_prev),
             beginning_of_interaction = ((status_new == "on") & (status_prev == "off")),
             end_of_interaction = ((status_new == "off") & (status_prev == "on")),
             time = POSIXct, 
             prev_time = lag(time),
             duration_since_last_change = time - prev_time,
             status_since_last_change = status_prev) %>%
      mutate(date_for_sleep = ifelse(hour_of_day>=16, date+1, date),
             date_for_sleep = as.Date(date_for_sleep))
    
    ## messages
  } else if(table_type == "messages"){
    data %<>% arrange(timestamp)
    data %<>% expand_timestamp_metrics()
    
    ## default
  } else {
    warning(str_glue("No matching preprocess step found for table type {table_type}"))
  }
  
  saveRDS(data, compress="gzip", file = output_filepath)
  
  
  return(output_filepath)
}


