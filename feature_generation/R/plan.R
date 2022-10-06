cfg_raw = list(
  # Freeze date for querying data from AWARE databases
  freeze_datetime = as.POSIXct("2021-05-11 23:59:59", tz="America/Los_Angeles"),
  freeze_datetime_db = NULL,
  # Freeze date to be convered to AWARE timestamp
  freeze_timestamp_aware = NULL,
  freeze_string = NULL,
  overwrite = FALSE
)
# Get OS this R process is running on
sys_os = Sys.info()['sysname']
print(paste0("Running on OS: ", sys_os))

#Configure data directory depending where the script is running
if(sys_os == "Windows") {  #Running on local PC
    data_dir_prefix = "data_20210511"
} else if(sys_os == "Linux") {  #Running on Hoffman 
    data_dir_prefix = "../STAND/data/data_by_subject_20210511"
} else{  # All other OSs
    data_dir_prefix = "data_20210511"
}

raw_data_dir = file.path(data_dir_prefix, "raw")
prep_data_dir = file.path(data_dir_prefix, "prep")
feat_data_dir = file.path(data_dir_prefix, "feat")
summ_data_dir = file.path(data_dir_prefix, "summ")
meta_data_dir = file.path(data_dir_prefix, "meta")


raw_data_sub_dir = "raw"
features_sub_dir = "features"
processed_data_sub_dir = "processed"

AWAREdb = "AWARE"
REDCAPdb = "REDCAP"

# Tables to pull AWARE data from
aware_table_names_and_ids = c(
  "applications_foreground", "apps",
  "plugin_google_activity_recognition", "gact",
  "plugin_ios_activity_recognition", "iact",
  "calls", "call",
  "messages", "msgs",
  "screen", "scrn",
  "locations", "locn",
  "light", "lite",
  "battery", "batt",
  "battery_charges", "chrg",
  "battery_discharges", "dchr",
  "network_traffic", "ntwk",
  "plugin_pedometer", "pedm",
  "accelerometer", "accl"
  "push_notification", "notf"
)

# Generate reference table guiding where to process data for each table
aware_table_ids <- aware_table_names_and_ids[c(FALSE,TRUE)]
aware_table_names <- aware_table_names_and_ids[c(TRUE,FALSE)]

table_info = tibble(id = aware_table_ids) %>%
  mutate(name = aware_table_names) %>%
  column_to_rownames(var ="id")

cat_table = "cat"

config = process_config(cfg_raw)





# Load list of subject_ids to run analyses on
target_subject_ids = as.character(read_csv("input/target_subject_ids.csv", col_types = cols(.default = col_character()))$subject_id)
# target_subject_ids = c('5554')  # Select an individual subject for processing, e.g. to troubleshoot errors

# Artificially reduce the number of subjects during development and testing
# target_subject_ids = target_subject_ids[1:2]

# Drake plan
pull_subject_ids <- 
  drake_plan(
    
    subject2ids_raw = pull_subject2device(target_db = REDCAPdb,
                                          max_timestamp = config$freeze_datetime_db),
    
    subject2ids_csv = write_csv(subject2ids_raw, file_out("output/subject2device.csv")),

  )

pull_data <-
  drake_plan(
    # Limit the number of branches during development and testing (static branches only)
    # max_expand = 50,
    
    # Generate full config object from skeleton
    config = process_config(cfg_raw),
    
    # Get subject_id to device_id mapping for all subjects after freeze date
    table_info_csv = write_csv(table_info, file_out("output/table_info.csv")),
    
    # Load mapping from subject_ids to device_ids
    subject2ids_raw = read_csv(file_in("output/subject2device.csv")),
    
    subject2ids = generate_subject2ids(subject2ids_raw),
    
    
    # Download raw AWARE data using static branching across tables and subject_ids
    aware_raw_data_files = target(
      # Don't overwrite if file exists
      download_all_tables(target_db = AWAREdb, # Name of DB for accessing env credentials
                          table_names = aware_table_names, # info for target tables to pull data from
                          device_ids = unique(subject2ids[[subject_id]]$device_id), # list of device_ids to download data for
                          subject_id = subject_id, # target subject id for this download
                          download_dir = raw_data_dir,
                          max_timestamp = config$freeze_timestamp_aware,
                          overwrite = FALSE), # max date from which to retrieve data
      transform = map(subject_id = !!target_subject_ids),
      format = "file"
    )
    
  )

preprocess_data <- 
  drake_plan(
    # Limit the number of branches during development and testing (static branches only)
    # max_expand = 10,
    
    prep_data = target(
      preprocess_subject_data_2(input_dir = raw_data_dir,
                                output_dir = prep_data_dir,
                                subject_id,
                                tables = NULL,
                                overwrite = TRUE),
      transform = map(subject_id = !!target_subject_ids),
      format = "file"
    ),
    
    print_file = target(
      print(prep_data),
      transform = map(prep_data)
    ),
  )

extract_features <- 
  drake_plan(
    # Limit the number of branches during development and testing (static branches only)
    # max_expand = 10,
    
    feat = target(
      generate_features_2(input_dir = prep_data_dir,
                          output_dir = feat_data_dir,
                          meta_dir = meta_data_dir,
                          subject_id,
                          overwrite = TRUE),
      transform = map(subject_id = !!target_subject_ids),
      format = "file"
    ),
    
    combine = target(
      combine_features_across_subjects(feat,
                                       input_dir = feat_data_dir,
                                       output_path = file.path(summ_data_dir, str_glue("features.chris.{Sys.Date()}.rds")),
                                       meta_dir = meta_data_dir,
                                       overwrite = TRUE),
      transform = combine(feat),
      format = "file"
    ),
    
    
  )
  
