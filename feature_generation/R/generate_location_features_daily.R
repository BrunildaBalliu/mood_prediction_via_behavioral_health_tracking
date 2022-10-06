resample_trajectory <- function(trajectory, duration_sec = 3*60) {
  
  
  min_time = ceiling_date(min(trajectory$time), unit = "minutes")
  max_time = floor_date(max(trajectory$time), unit = "minutes")
  
  if(min_time > max_time)
    min_time = max_time
  
  trajectory = trajectory %>% mutate(resample_timepoint = FALSE)
  
  if(as.numeric(max_time, units = "secs") - as.numeric(min_time, units ="secs") >= duration_sec)
    generated_timepoints = seq(from = min_time, to = max_time, by = duration_sec)
  else
    generated_timepoints = c(min_time)
  
  generated_trajectory = tibble(time = generated_timepoints) %>% mutate(resample_timepoint = TRUE)
  
  combined_trajectory = bind_rows(trajectory, generated_trajectory)
  
  z = zoo(combined_trajectory, combined_trajectory$time)
  lat_int = na.approx(z$lat, rule = 2) 
  lon_int = na.approx(z$lon, rule = 2) 
  
  resampled_trajectory = as_tibble(coredata(z)) %>% mutate(time = as.POSIXct(time, tz = "America/Los_Angeles"))
  resampled_trajectory = cbind(resampled_trajectory, lon_int, lat_int)
  resampled_trajectory = resampled_trajectory %>% dplyr::filter(resample_timepoint == TRUE)
  
  resampled_trajectory = resampled_trajectory %>%
    select(time, lon_int, lat_int) %>%
    rename(lon = lon_int, lat = lat_int) %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat))
  return(resampled_trajectory)
}


expand_trajectory_metrics = function(trajectory, speed_threshold = 0.7){
  # speed threshold of 0.7 is 50% of average walking speed, i.e. stationary for >50% of the interval
  result = trajectory %>%
    mutate(hour = lubridate::hour(time)) %>%
    mutate(is_night = (hour>=0 & hour<8),
           is_day = (hour>=8 & hour<16),
           is_evening = (hour>=16 & hour<24)) %>%
    mutate(prev_time = dplyr::lag(time),
           prev_lon = dplyr::lag(lon),
           prev_lat = dplyr::lag(lat)) %>%
    mutate(next_time = dplyr::lead(time),
           next_lon = dplyr::lead(lon),
           next_lat = dplyr::lead(lat)) %>%
    mutate(prev_duration = time - prev_time,
           next_duration = next_time - time) %>%
    mutate(ave_duration = (prev_duration+next_duration)/2) %>%
    mutate(prev_distance = distHaversineVect(prev_lon, prev_lat, lon, lat),
           next_distance = distHaversineVect(next_lon, next_lat, lon, lat)) %>%
    mutate(prev_speed = prev_distance / as.numeric(prev_duration, units = "secs"),
           next_speed = next_distance / as.numeric(next_duration, units = "secs")) %>%
    mutate(is_stationary = prev_speed <= speed_threshold) 
  
  return(result)
}

generate_location_features_daily <- function(data) {
  

  cluster_dist_threshold = 800 #meters
  min_stationary_threshold = 15*60 #seconds
  
  features = list()
  
  # Basic features
  # weekday = unique(data$weekday)
  # features = log_feature(features, weekday)
  
  # is_weekend = unique(data$is_weekend)
  # features = log_feature(features, is_weekend)
  
  n_locations_obs = nrow(data)
  features = log_feature(features, n_locations_obs)
  
  
  if(n_locations_obs>0){
    
    
    avg_location_freq_m = mean(data$time_duration_m)
    features = log_feature(features, avg_location_freq_m)
    
    variance_latitude = var(data$double_latitude)
    features = log_feature(features, variance_latitude)
    
    variance_longitude = var(data$double_longitude)
    features = log_feature(features, variance_longitude)
    
    
    # Trajectory + location features
    
    original_trajectory = data %>% 
      select(POSIXct, double_longitude, double_latitude) %>%
      rename(time = POSIXct, lat = double_latitude, lon = double_longitude ) %>%
      distinct()
    
    QC.first_observation.time = min(original_trajectory$time) 
    date(QC.first_observation.time) = "1970-01-01"
    features = log_feature(features, QC.first_observation.time)
    first_observation.hrsam = as.numeric(QC.first_observation.time - as.POSIXct("1970-01-01 00:00:00", tz = "America/Los_Angeles"), units = "hours")
    features = log_feature(features, first_observation.hrsam)
    
    QC.last_observation.time = max(original_trajectory$time)
    date(QC.last_observation.time) = "1970-01-01"
    features = log_feature(features, QC.last_observation.time)
    last_observation.hrsam = as.numeric(QC.last_observation.time - as.POSIXct("1970-01-01 00:00:00", tz = "America/Los_Angeles"), units = "hours")
    features = log_feature(features, last_observation.hrsam)
    
    resampled_trajectory = resample_trajectory(original_trajectory, duration_sec = 3*60)
    
    
    original_trajectory = expand_trajectory_metrics(original_trajectory)
    resampled_trajectory = expand_trajectory_metrics(resampled_trajectory)
    
    stationary_points = original_trajectory %>% dplyr::filter(is_stationary == TRUE)
    count_stationary = nrow(stationary_points)
    if(count_stationary>=2){
      
      if(count_stationary >= 10000){
        warning(str_glue("{count_stationary} stationary points in GPS data, downsampling to improve speed..."))
        stationary_points = stationary_points %>% mutate(time_rounded = round_date(time, unit = "minute")) 
        stationary_points %<>% group_by(time_rounded) %>% summarize_all(mean)
      }
      
      stops_distm <- as.dist(distm(cbind(stationary_points$lon,stationary_points$lat),fun=distHaversine))
      
      stops_hclust <- hclust(stops_distm, method = "complete")
      
      clusters = cutree(stops_hclust, h=cluster_dist_threshold)
      
      stationary_points$cluster = clusters 
      
      cluster_characteristics = stationary_points %>% 
        group_by(cluster) %>%
        summarize(lon_mean = mean(lon),
                  lat_mean = mean(lat),
                  lon_median = median(lon),
                  lat_median = median(lat),
                  total_duration = sum(prev_duration)) %>%
        dplyr::filter(as.numeric(total_duration, units = "secs") >= min_stationary_threshold) %>%
        mutate(delta_mean_median = distHaversineVect(lon_mean, lat_mean, lon_median, lat_median)) 
      
      if(nrow(cluster_characteristics)>=1){
        cluster_dist = distm(cbind(resampled_trajectory$lon, resampled_trajectory$lat),
                             cbind(cluster_characteristics$lon_mean, cluster_characteristics$lat_mean),
                             fun=distHaversine)
        
        cluster_mapping = as_tibble(cluster_dist) %>%
          rownames_to_column(var = "point_id") %>%
          mutate(point_id = as.numeric(point_id)) %>%
          pivot_longer(cols = starts_with("V"), names_to = "cluster", values_to = "distance") %>%
          group_by(point_id) %>%
          slice(which.min(distance)) %>%
          rename(closest_cluster = cluster, distance_to_closest_cluster = distance) %>%
          mutate(closest_cluster = as.numeric(str_remove(closest_cluster, "V"))) %>%
          mutate(belongs_to_cluster = ifelse(distance_to_closest_cluster<cluster_dist_threshold/2, closest_cluster, -1)) 
        
        
        
        resampled_trajectory = resampled_trajectory %>%
          mutate(cluster = cluster_mapping$belongs_to_cluster)
        
        home_cluster = resampled_trajectory %>%
          dplyr::filter(is_night==TRUE) %>%
          dplyr::filter(cluster >= 0) %>%
          group_by(cluster) %>% 
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          slice(which.max(total_duration)) %>%
          pull(cluster)
        
        if(length(home_cluster) == 1){
          
          
          home_lon = cluster_characteristics %>% dplyr::filter(cluster == home_cluster)  %>% pull(lon_mean)
          home_lat = cluster_characteristics %>% dplyr::filter(cluster == home_cluster)  %>% pull(lat_mean)
          
          features = log_feature(features, home_lon)
          features = log_feature(features, home_lat)
          
          # Time spent at home metrics
          
          
          time_spent_at_home.total = resampled_trajectory %>%
            dplyr::filter(cluster == home_cluster) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration)
          time_spent_at_home.total.h = as.numeric(time_spent_at_home.total, units = "hours")
          features = log_feature(features, time_spent_at_home.total.h) 
          
          time_spent_at_home.night = resampled_trajectory %>%
            dplyr::filter(cluster == home_cluster) %>%
            dplyr::filter(is_night == TRUE) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration)
          time_spent_at_home.night.h = as.numeric(time_spent_at_home.night, units = "hours")
          features = log_feature(features, time_spent_at_home.night.h)
          
          time_spent_at_home.day = resampled_trajectory %>%
            dplyr::filter(cluster == home_cluster) %>%
            dplyr::filter(is_day == TRUE) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration)
          time_spent_at_home.day.h = as.numeric(time_spent_at_home.day, units = "hours")
          features = log_feature(features, time_spent_at_home.day.h) 
          
          time_spent_at_home.evening = resampled_trajectory %>%
            dplyr::filter(cluster == home_cluster) %>%
            dplyr::filter(is_evening == TRUE) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration)
          time_spent_at_home.evening.h = as.numeric(time_spent_at_home.evening, units = "hours")
          features = log_feature(features, time_spent_at_home.evening.h) 
          
          # Time spent at home (percentage) metrics
          
          time_spent_at_home.total.pct = as.numeric(time_spent_at_home.total, units = "secs") /
            resampled_trajectory %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration) %>%
            as.numeric(units="secs")
          features = log_feature(features, time_spent_at_home.total.pct) 
          
          time_spent_at_home.night.pct = as.numeric(time_spent_at_home.night, units = "secs") /
            resampled_trajectory %>%
            dplyr::filter(is_night == TRUE) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration) %>%
            as.numeric(units="secs")
          features = log_feature(features, time_spent_at_home.night.pct) 
          
          time_spent_at_home.day.pct = as.numeric(time_spent_at_home.day, units = "secs") /
            resampled_trajectory %>%
            dplyr::filter(is_day == TRUE) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration) %>%
            as.numeric(units="secs")
          features = log_feature(features, time_spent_at_home.day.pct) 
          
          time_spent_at_home.evening.pct = as.numeric(time_spent_at_home.evening, units = "secs") /
            resampled_trajectory %>%
            dplyr::filter(is_evening == TRUE) %>%
            summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
            pull(total_duration) %>%
            as.numeric(units="secs")
          features = log_feature(features, time_spent_at_home.evening.pct) 
          
        }
        # Number of location metrics
        
        num_locations.total = resampled_trajectory %>%
          dplyr::filter(cluster >=0 ) %>%
          pull(cluster) %>%
          unique() %>%
          length()
        features = log_feature(features, num_locations.total) 
        
        num_locations.night = resampled_trajectory %>%
          dplyr::filter(cluster >=0 ) %>%
          dplyr::filter(is_night == TRUE) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          dplyr::filter(as.numeric(total_duration, units = "secs") >= min_stationary_threshold) %>%
          pull(cluster) %>%
          unique() %>%
          length()
        features = log_feature(features, num_locations.night) 
        
        num_locations.day = resampled_trajectory %>%
          dplyr::filter(cluster >=0 ) %>%
          dplyr::filter(is_day == TRUE) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          dplyr::filter(as.numeric(total_duration, units = "secs") >= min_stationary_threshold) %>%
          pull(cluster) %>%
          unique() %>%
          length()
        features = log_feature(features, num_locations.day) 
        
        num_locations.evening = resampled_trajectory %>%
          dplyr::filter(cluster >=0 ) %>%
          dplyr::filter(is_evening == TRUE) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          dplyr::filter(as.numeric(total_duration, units = "secs") >= min_stationary_threshold) %>%
          pull(cluster) %>%
          unique() %>%
          length()
        features = log_feature(features, num_locations.evening) 
        
        # Entropy metrics
        location_entropy.total = resampled_trajectory %>%
          dplyr::filter(cluster >= 0) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          mutate(total_duration.secs = as.numeric(total_duration, units = "secs")) %>%
          mutate(total_duration.pct = total_duration.secs / sum(total_duration.secs)) %>%
          mutate(entropy_component = -log(total_duration.pct)*total_duration.pct) %>%
          pull(entropy_component) %>%
          sum()
        features = log_feature(features, location_entropy.total) 
        
        location_entropy.total.norm = normalize_location_entropy(location_entropy.total, num_locations.total)
        features = log_feature(features, location_entropy.total.norm) 
        
        location_entropy.night = resampled_trajectory %>%
          dplyr::filter(cluster >= 0) %>%
          dplyr::filter(is_night == TRUE) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          mutate(total_duration.secs = as.numeric(total_duration, units = "secs")) %>%
          mutate(total_duration.pct = total_duration.secs / sum(total_duration.secs)) %>%
          mutate(entropy_component = -log(total_duration.pct)*total_duration.pct) %>%
          pull(entropy_component) %>%
          sum()
        features = log_feature(features, location_entropy.night) 
        
        location_entropy.night.norm = normalize_location_entropy(location_entropy.night, num_locations.night)
        features = log_feature(features, location_entropy.night.norm) 
        
        location_entropy.day = resampled_trajectory %>%
          dplyr::filter(cluster >= 0) %>%
          dplyr::filter(is_day == TRUE) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          mutate(total_duration.secs = as.numeric(total_duration, units = "secs")) %>%
          mutate(total_duration.pct = total_duration.secs / sum(total_duration.secs)) %>%
          mutate(entropy_component = -log(total_duration.pct)*total_duration.pct) %>%
          pull(entropy_component) %>%
          sum()
        features = log_feature(features, location_entropy.day) 
        
        location_entropy.day.norm = normalize_location_entropy(location_entropy.day, num_locations.day)
        features = log_feature(features, location_entropy.day.norm) 
        
        location_entropy.evening = resampled_trajectory %>%
          dplyr::filter(cluster >= 0) %>%
          dplyr::filter(is_evening == TRUE) %>%
          group_by(cluster) %>%
          summarize(total_duration = sum(prev_duration, na.rm = TRUE)) %>%
          mutate(total_duration.secs = as.numeric(total_duration, units = "secs")) %>%
          mutate(total_duration.pct = total_duration.secs / sum(total_duration.secs)) %>%
          mutate(entropy_component = -log(total_duration.pct)*total_duration.pct) %>%
          pull(entropy_component) %>%
          sum()
        features = log_feature(features, location_entropy.evening) 
        
        location_entropy.evening.norm = normalize_location_entropy(location_entropy.evening, num_locations.evening)
        features = log_feature(features, location_entropy.evening.norm) 
      }
    }
  }
  return(as_tibble(features))
}

normalize_location_entropy <- function(entropy, number_of_states){
  if(number_of_states == 0){
    return(NA)
  }
  else if(number_of_states == 1) {
    return(0)
  }
  else {
    return(entropy/log(number_of_states))
  }
}
