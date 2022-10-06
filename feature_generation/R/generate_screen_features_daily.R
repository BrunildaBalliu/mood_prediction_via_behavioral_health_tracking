generate_screen_features_daily <- function(data){
  
  features = list()
  
  n_screen_obs_day = nrow(data)
  features = log_feature(features, n_screen_obs_day)
  
  if(n_screen_obs_day > 2){
    
    time_first_interaction.time = data %>%
      filter(beginning_of_interaction == TRUE, hour_of_day>=4) %>%
      slice(which.min(time)) %>%
      pull(time)
    date(time_first_interaction.time) = "1970-01-01"
    QC.time_first_interaction.time = time_first_interaction.time
    features = log_feature(features, QC.time_first_interaction.time)
    
    time_first_interaction.hrsam = as.numeric(time_first_interaction.time - as.POSIXct("1970-01-01 00:00:00", tz = "America/Los_Angeles"), units = "hours")
    features = log_feature(features, time_first_interaction.hrsam)
    
    n_interactions = data %>%
      filter(beginning_of_interaction == TRUE) %>%
      summarize(count_interactions = n()) %>%
      pull(count_interactions)
    features = log_feature(features, n_interactions)
    
  }
  
  return(as_tibble(features))
  
}


generate_screen_features_nightly <- function(data){
  features = list()
  
  n_screen_obs_night = nrow(data)
  features = log_feature(features, n_screen_obs_night)
  
  n_interactions_night = data %>%
    filter(beginning_of_interaction == TRUE) %>%
    summarize(count_interactions = n()) %>%
    pull(count_interactions)
  features = log_feature(features, n_interactions_night)
  
  longest_phone_off_period = data %>%
    select(subject_id, timestamp, unix_timestamp, date, date_for_sleep, time_of_day, screen_status, status_new, status_prev, status_changed, beginning_of_interaction, end_of_interaction, time) %>%
    filter(status_changed == TRUE | is.na(status_changed)) %>%
    mutate(prev_time = lag(time)) %>%
    filter(beginning_of_interaction == TRUE) %>%
    mutate(duration_since_last_change = time - prev_time) %>%
    slice(which.max(duration_since_last_change))
  
  if(nrow(longest_phone_off_period)>0){
    
    longest_phone_off_duration.h = as.numeric(longest_phone_off_period %>% pull(duration_since_last_change))/(60*60)
    features = log_feature(features, longest_phone_off_duration.h)
    
    placeholder_date = as.Date("1970-01-01")
    
    
    longest_phone_off_begin.time = longest_phone_off_period %>% pull(prev_time)
    if(hour(longest_phone_off_begin.time)>=16){
      date(longest_phone_off_begin.time) = placeholder_date - 1
    } else{
      date(longest_phone_off_begin.time) = placeholder_date
    }
    QC.longest_phone_off_begin.time = longest_phone_off_begin.time
    features = log_feature(features, QC.longest_phone_off_begin.time)
    
    longest_phone_off_begin.hrsam = as.numeric(longest_phone_off_begin.time - as.POSIXct("1970-01-01 00:00:00", tz = "America/Los_Angeles"), units = "hours")
    features = log_feature(features, longest_phone_off_begin.hrsam)
    
    
    longest_phone_off_end.time = longest_phone_off_period %>% pull(time)
    if(hour(longest_phone_off_end.time)>=16){
      date(longest_phone_off_end.time) = placeholder_date - 1
    } else{
      date(longest_phone_off_end.time) = placeholder_date
    }  
    QC.longest_phone_off_end.time = longest_phone_off_end.time
    features = log_feature(features, QC.longest_phone_off_end.time)
    
    longest_phone_off_end.hrsam = as.numeric(longest_phone_off_end.time - as.POSIXct("1970-01-01 00:00:00", tz = "America/Los_Angeles"), units = "hours")
    features = log_feature(features, longest_phone_off_end.hrsam)
    
  }
  # Base variables for time processing
  timezone = attr(data$time, "tzone")
  
  new_day_date = unique(data$date_for_sleep)
  start_time = "00:00:00"
  end_time = "00:08:00"
  start_datetime = max(data$time)
  date(start_datetime) = new_day_date
  hour(start_datetime) = 0
  minute(start_datetime) = 0
  second(start_datetime) = 0
  
  end_datetime = max(data$time)
  date(end_datetime) = new_day_date
  hour(end_datetime) = 8
  minute(end_datetime) = 0
  second(end_datetime) = 0
  
  phone_period_12_8 = data %>% 
    filter((hour(time)>=0 & hour(time)<8) |
           (hour(prev_time)>=0 & hour(prev_time)<8) |
           (hour(prev_time)>=16 & hour(time)>=8 & hour(time) <16)) %>%
    mutate(prev_time = replace(prev_time, prev_time<start_datetime, start_datetime)) %>% # Cap prev_time (beginning of interval) at beginning of period
    mutate(time = replace(time, time>end_datetime, end_datetime)) %>% # Cap time (end of interval) at end of period
    mutate(duration_since_last_change = time - prev_time) %>%
    group_by(status_since_last_change) %>%
    summarize(total_time_h = sum(as.numeric(duration_since_last_change, units = "hours"))) %>%
    mutate(total_time_all_categories = sum(total_time_h),
           total_time_pct = total_time_h/sum(total_time_all_categories))
  
  phone_on_duration = phone_period_12_8 %>% slice(which(status_since_last_change=="on")) %>% pull(total_time_h)
  phone_on_duration = ifelse(length(phone_on_duration)==0,0,phone_on_duration)
  phone_off_duration = phone_period_12_8 %>% slice(which(status_since_last_change=="off")) %>% pull(total_time_h)
  phone_off_duration = ifelse(length(phone_off_duration)==0,0,phone_off_duration)
  
  phone_dummy_duration_12_8.h = phone_period_12_8 %>% slice(1) %>% pull(total_time_all_categories)
  features = log_feature(features, phone_dummy_duration_12_8.h)
  
  phone_on_duration_12_8.h = phone_on_duration
  features = log_feature(features, phone_on_duration_12_8.h)
  
  phone_off_duration_12_8.h = phone_off_duration
  features = log_feature(features, phone_off_duration_12_8.h)
  
  phone_on_duration_12_8.pct = phone_on_duration / (phone_on_duration + phone_off_duration)
  features = log_feature(features, phone_on_duration_12_8.pct)
  
  if(n_interactions_night > 2){
    # Nadir of interactions calculations
    day_placeholder = unique(data$date_for_sleep)[1]
    dens = density(as.numeric(data$time))
    den_tbl = tibble(time_as_numeric = dens$x, density = dens$y) %>%
      mutate(time = as.POSIXct(time_as_numeric, origin = "1970-01-01 00:00:00 UTC")) %>%
      filter((date(time)==day_placeholder & hour(time)<12) |
               (date(time)==day_placeholder-1 & hour(time)>=20))
    time_nadir_interactions.time = den_tbl %>% slice(which.min(density)) %>% pull(time)
    date(time_nadir_interactions.time) = "1970-01-01"
    attr(time_nadir_interactions.time, "tzone") = timezone
    QC.time_nadir_interactions.time = time_nadir_interactions.time
    features = log_feature(features, QC.time_nadir_interactions.time)
    
    time_nadir_interactions.hrsam = as.numeric(time_nadir_interactions.time - as.POSIXct("1970-01-01 00:00:00", tz = "America/Los_Angeles"), units = "hours")
    features = log_feature(features, time_nadir_interactions.hrsam)
  }
  
  return(as_tibble(features))
}