generate_messages_features_daily <- function(data){
  features = list()
  
  QC.has_messages_data = TRUE
  features = log_feature(features, QC.has_messages_data)
  
  n_messages_obs = nrow(data)
  features = log_feature(features, n_messages_obs)
  
  n_messages_in = data %>% filter(message_type == 1) %>% nrow()
  features = log_feature(features, n_messages_in)
  
  n_messages_out = data %>% filter(message_type == 2) %>% nrow()
  features = log_feature(features, n_messages_out)
  
  messages_out.pct = n_messages_out / (n_messages_out + n_messages_in) 
  features = log_feature(features, messages_out.pct)
  
  return(as_tibble(features))
}