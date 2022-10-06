generate_calls_features_daily <- function(data){
  features = list()
  
  QC.has_calls_data = TRUE
  features = log_feature(features, QC.has_calls_data)
  
  n_calls_obs = nrow(data)
  features = log_feature(features, n_calls_obs)
  
  n_calls_in = data %>% filter(call_category == "incoming") %>% nrow()
  features = log_feature(features, n_calls_in)
  
  n_calls_out = data %>% filter(call_category == "outgoing") %>% nrow()
  features = log_feature(features, n_calls_out)
  
  n_calls_completed = n_calls_in + n_calls_out
  features = log_feature(features, n_calls_completed)
  
  n_calls_missed = data %>% filter(call_category == "missed") %>% nrow()
  features = log_feature(features, n_calls_missed)
  
  n_calls_outmissed = data %>% filter(call_category == "outgoing_missed") %>% nrow()
  features = log_feature(features, n_calls_outmissed)
  
  completed_calls_out.pct = n_calls_out / (n_calls_out + n_calls_in)
  features = log_feature(features, completed_calls_out.pct)
  
  contact_attempts_out.pct = (n_calls_out + n_calls_outmissed) / (n_calls_obs)
  features = log_feature(features, contact_attempts_out.pct)
  
  duration_calls_in.m = data %>% filter(call_category == "incoming") %>% pull(call_duration) %>% sum() / 60
  features = log_feature(features, duration_calls_in.m)
  
  duration_calls_out.m = data %>% filter(call_category == "outgoing") %>% pull(call_duration) %>% sum() / 60
  features = log_feature(features, duration_calls_out.m)
  
  duration_calls_total.m = duration_calls_in.m + duration_calls_out.m
  features = log_feature(features, duration_calls_total.m)
  
  duration_calls_ave.m = duration_calls_total.m / n_calls_completed
  features = log_feature(features, duration_calls_ave.m)
  
  duration_calls_out.pct = duration_calls_out.m / (duration_calls_in.m + duration_calls_out.m)
  features = log_feature(features, duration_calls_out.pct)
  
  n_distinct_trace = data %>% select(trace) %>% distinct() %>% nrow()
  features = log_feature(features, n_distinct_trace)
  
  
  return(as_tibble(features))
}