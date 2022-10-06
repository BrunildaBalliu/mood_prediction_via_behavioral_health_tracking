generate_features_2 <- function(input_dir,
                                output_dir,
                                meta_dir,
                                subject_id,
                                feat_sets = NULL,
                                overwrite = TRUE){
  
  gen_path = function(input_dir, subject_id, table, extension){
    return(file.path(input_dir,paste(subject_id,table,extension, sep=".")))
  }
  
  features = NULL
  
  subject2device_detailed = readRDS(file.path(meta_dir,"subject2device_detailed.rds"))
  device2os = subject2device_detailed %>% 
    select(device_id, manufacturer) %>%
    mutate(os = ifelse(manufacturer == "Apple", "ios", "android")) %>%
    distinct()

  # Location features
  locations_path = gen_path(input_dir, subject_id, "locations", "rds")
  locations_data = readRDS(locations_path)
  if(nrow(locations_data) > 0){
    
    locations_daily = locations_data %>% 
      group_by(date)
    
      
    locations_features_daily = locations_daily %>%  
      do(generate_location_features_daily(.))
    
    rm(locations_daily)
    
    features = merge_features(features, locations_features_daily)
  
  }
  rm(locations_data)
  # dbgvar(locations_data)
  
  
  # Screen features
  screen_path = gen_path(input_dir, subject_id, "screen", "rds")
  screen_data = readRDS(screen_path)
  
  if(nrow(screen_data) > 0){
    # Screen - day
    screen_daily = screen_data %>% 
      group_by(date)
    
    screen_features_daily = screen_daily %>%  
      do(generate_screen_features_daily(.))
    
    rm(screen_daily)
    features = merge_features(features, screen_features_daily, by = "date")
    
    # Screen - night(sleep)
    screen_nightly = screen_data %>% 
      group_by(date_for_sleep)
    
    screen_features_nightly = screen_nightly %>%  
      do(generate_screen_features_nightly(.))
    
    rm(screen_nightly)
    features = merge_features(features, screen_features_nightly, by = c("date" = "date_for_sleep"))
  }
  rm(screen_data)
  

  
  # Calls features
  calls_path = gen_path(input_dir, subject_id, "calls", "rds")
  calls_data = readRDS(calls_path)
  
  calls_data %<>% left_join(device2os, by = "device_id") %>%
    relocate(manufacturer, .after=device_id) %>%
    relocate(os, .after=manufacturer)
  calls_data %<>% 
    group_by(trace) %>%
    arrange(timestamp) %>% 
    mutate(prev1_type = lag(call_type,1), 
           prev2_type = lag(call_type,2)) %>%
    mutate(call_sequence = str_c(str_replace_na(call_type, replacement = ""),
                                 str_replace_na(prev1_type, replacement = ""),
                                 str_replace_na(prev2_type, replacement = ""))) %>%
    relocate(prev1_type, .after = call_type) %>%
    relocate(prev2_type, .after = prev1_type) %>%
    relocate(call_sequence, .after = prev2_type)
  calls_data %<>%
    mutate(call_category = case_when(
      os == "ios" & call_sequence == "421" ~ "incoming",
      os == "ios" & call_sequence == "423" ~ "outgoing",
      os == "ios" & call_sequence == "41" ~ "missed",
      os == "ios" & call_sequence == "43" ~ "outgoing_missed",
      os == "android" & call_type == "1" ~ "incoming",
      os == "android" & call_type == "2" ~ "outgoing",
      os == "android" & call_type == "3" ~ "missed")) %>%
    relocate(call_category, .after = call_sequence) %>%
    filter(!is.na(call_category))
  
  if(nrow(calls_data) > 0){
    calls_daily = calls_data %>%
      group_by(date)

    calls_features_daily = calls_daily %>%
      do(generate_calls_features_daily(.))

    rm(calls_daily)
    features = merge_features(features, calls_features_daily, by = "date")
  }
  rm(calls_data)
  
  
  # Messages features
  messages_path = gen_path(input_dir, subject_id, "messages", "rds")
  messages_data = readRDS(messages_path)
  if(nrow(messages_data) > 0){
    messages_daily = messages_data %>%
      group_by(date)


    messages_features_daily = messages_daily %>%
      do(generate_messages_features_daily(.))

    rm(messages_daily)
    features = merge_features(features, messages_features_daily, by = "date")
  }
  rm(messages_data)

  # Activity features
  
  
  

  if(is.null(features)){
    features = tibble(subject_id, date = NA)
  } else {
    features %<>% mutate(subject_id = subject_id) %>% relocate(subject_id)
  }
  
  output_path_csv = gen_path(output_dir, subject_id, "chris", "csv")
  write_csv(features, output_path_csv)

  output_path_rds = gen_path(output_dir, subject_id, "chris", "rds")
  saveRDS(features, output_path_rds)
  
  return(output_path_rds)
}




generate_features <- function(prep_data){
  
  # Location features
  locations_daily = prep_data$locations %>% 
    group_by(date)
    
  location_features_daily = locations_daily %>%  
    do(generate_location_features_daily(.))
  features = location_features_daily
  
  # Screen features
  screen_daily = prep_data$locations %>% 
    group_by(date)
  
  # screen_features_daily = screen_daily %>%  
  #   do(generate_screen_features_daily(.))
  # features = full_join(features, screen_  features_daily, by = c("date" = "date"))
  
  # Call features
  calls_daily = prep_data$locations %>% 
    group_by(date)
  
  # Activity features
  plugin_google_activity_recognition = prep_data$locations %>% 
    group_by(date)
  
  plugin_ios_activity_recognition = prep_data$locations %>% 
    group_by(date)
  
  
  
  return(features)
}