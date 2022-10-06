pull_subject2device <- function(target_db,
                                max_timestamp) {

  query <- sprintf(
  "select
    study_subjects.subject_id,
    study_subjects.study_subject_id,
    study_subjects.study_name,
    study_subjects.study_identifier,
    study_subjects.cat_subject_id,
    aware_devices.device_id,
    study_subjects.created_at,
    study_subjects.updated_at,
    study_subjects.exclude,
    study_subjects.consent
  from
    study_subjects
  left join
    aware_devices on study_subjects.subject_id = aware_devices.subject_id
  where 
    study_subjects.exclude = 0 AND
    study_subjects.consent = 1 AND
    aware_devices.device_id IS NOT NULL AND
    study_subjects.created_at <= '%s';",
  max_timestamp)
  
  result <- run_query_on_db(target_db, query)
  
  return(result)
}
