convert_id_table_to_map <- function(table,
                                    subjectid_col,
                                    deviceid_col) {
  
  print(c(subjectid_col, deviceid_col))
  map <- table %>% 
    select(!!subjectid_col, !!deviceid_col) %>%
    rename(subject_id = !!subjectid_col) %>%
    rename(device_id = !!deviceid_col) %>%
    arrange(subject_id, device_id) %>%
    group_by(subject_id) 
  
  map <- map %>%
    group_split() %>%
    set_names(unlist(group_keys(map)))
  
  return(map)
  
}
