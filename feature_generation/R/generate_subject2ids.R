generate_subject2ids <- function(subject2ids_raw) {

  subject2ids <- subject2ids_raw %>%
    select(subject_id, study_subject_id, cat_subject_id, device_id, created_at) %>%
    group_by(subject_id) %>%
    arrange(created_at)
  
  subject2ids <- subject2ids %>%
    group_split() %>%
    set_names(unlist(group_keys(subject2ids)))
  
  return(subject2ids)
}
