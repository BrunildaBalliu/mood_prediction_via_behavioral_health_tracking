combine_features_across_subjects <- function(...,
                                             input_dir,
                                             meta_dir,
                                             output_path,
                                             filelist = NULL,
                                             overwrite = TRUE){

  subject2device_detailed = readRDS(file.path(meta_dir,"subject2device_detailed.rds"))

  subject2os = subject2device_detailed %>% 
    mutate(subject_id = as.character(subject_id)) %>%
    select(subject_id, device_id, manufacturer) %>%
    mutate(os = ifelse(manufacturer == "Apple", "ios", "android")) %>%
    select(subject_id, os) %>%
    distinct(subject_id, .keep_all = TRUE)
  

  
  
  combined_features = NULL
  if(!is.null(filelist)){
    feature_files = filelist
  }
  else{
    feature_files = list(...)
  }

  for(feature_file in feature_files){
    print(str_glue("Reading file: {feature_file}"))
    subject_features = readRDS(feature_file)
    subject_features %<>% filter(!is.na(date)) %>% arrange(date)
    if(nrow(subject_features)>0){
      cur_subject_id = unique(subject_features$subject_id)
      min_date = min(subject_features$date, na.rm = TRUE)
      max_date = max(subject_features$date, na.rm = TRUE)
      all_dates = seq(min_date, max_date, by = 1)
      date_tbl = tibble(date = all_dates)
      subject_features = left_join(date_tbl, subject_features, by = "date") %>% 
        mutate(subject_id = cur_subject_id) %>%
        relocate(subject_id)
      subject_features = transform_features(subject_features)
      
      if(is.null(combined_features)){
        combined_features = subject_features
      }else {
        combined_features = bind_rows(combined_features, subject_features)
      }
    }

  }

  combined_features %<>% 
    filter(!is.na(date)) %>%
    select(!starts_with("QC")) %>%
    mutate(weekday = weekdays(date),
           is_weekend = weekday %in% c("Saturday", "Sunday")) %>%
    relocate(weekday, .after=date) %>%
    relocate(is_weekend, .after=weekday) %>%
    left_join(subject2os, by = "subject_id") %>%
    relocate(os, .after = subject_id)
    
  
  saveRDS(combined_features, output_path)
  return(output_path)
}


transform_features <- function(features_table){
  
  # Helper functions
  mean_no_na = function(data){

    result = mean(data, na.rm = TRUE)
    result[is.nan(result)] = NA
    return(result)
  }
  
  var_no_na = function(data){
    result = var(data, na.rm = TRUE)
    result[is.nan(result)] = NA
    return(result)
  }
  
  # Main
  features_table = as.data.frame(features_table)
  cols = colnames(features_table)

  feature_cols = cols
  feature_cols = feature_cols[feature_cols != "subject_id" ]
  feature_cols = feature_cols[feature_cols != "date" ]

  col = "pos"
  
  for(col in feature_cols){
    
    col_class = class(features_table[[col]])[1]

    if(col_class == "logical") next
    
    features_table %<>% mutate("{col}.var3"          := rollapply(!!as.name(col),width =  3, FUN = var_no_na, align='right', fill=NA))
    features_table %<>% mutate("{col}.var7"          := rollapply(!!as.name(col),width =  7, FUN = var_no_na, align='right', fill=NA))
    features_table %<>% mutate("{col}.var14"         := rollapply(!!as.name(col),width =  14, FUN = var_no_na, align='right', fill=NA))
    features_table %<>% mutate("{col}.var30"         := rollapply(!!as.name(col),width =  30, FUN = var_no_na, align='right', fill=NA))
    
    
    
    # Rolling means
    features_table %<>% mutate("{col}.rmean3"         := rollapply(!!as.name(col),width =  3, FUN = mean_no_na, align='right', fill=NA))
    features_table %<>% mutate("{col}.rmean7"         := rollapply(!!as.name(col),width =  7, FUN = mean_no_na, align='right', fill=NA))
    features_table %<>% mutate("{col}.rmean14"        := rollapply(!!as.name(col),width = 14, FUN = mean_no_na, align='right', fill=NA))
    features_table %<>% mutate("{col}.rmean30"        := rollapply(!!as.name(col),width = 30, FUN = mean_no_na, align='right', fill=NA))
    
    # Change type of means
    if(col_class == "Date"){
      features_table[str_glue("{col}.rmean3")]  = as.Date(features_table[[str_glue("{col}.rmean3")]])
      features_table[str_glue("{col}.rmean7")]  = as.Date(features_table[[str_glue("{col}.rmean7")]])
      features_table[str_glue("{col}.rmean14")] = as.Date(features_table[[str_glue("{col}.rmean14")]])
      features_table[str_glue("{col}.rmean30")] = as.Date(features_table[[str_glue("{col}.rmean30")]])
    } 
    if(col_class == "POSIXct"){
      features_table[str_glue("{col}.rmean3")]  = as.POSIXct(features_table[[str_glue("{col}.rmean3")]], origin = "1970-01-01")
      features_table[str_glue("{col}.rmean7")]  = as.POSIXct(features_table[[str_glue("{col}.rmean7")]], origin = "1970-01-01")
      features_table[str_glue("{col}.rmean14")] = as.POSIXct(features_table[[str_glue("{col}.rmean14")]], origin = "1970-01-01")
      features_table[str_glue("{col}.rmean30")] = as.POSIXct(features_table[[str_glue("{col}.rmean30")]], origin = "1970-01-01")
    }
    
    
    
    # Delta between rolling means, ad deviation from recent baseline
    features_table %<>% mutate("{col}.d_mean_1v3"     := !!as.name(str_glue("{col}"))          - !!as.name(str_glue("{col}.rmean3")))
    features_table %<>% mutate("{col}.d_mean_3v7"     := !!as.name(str_glue("{col}.rmean3"))   - !!as.name(str_glue("{col}.rmean7")))
    features_table %<>% mutate("{col}.d_mean_7v14"    := !!as.name(str_glue("{col}.rmean7"))   - !!as.name(str_glue("{col}.rmean14")))
    features_table %<>% mutate("{col}.d_mean_14v30"   := !!as.name(str_glue("{col}.rmean14"))  - !!as.name(str_glue("{col}.rmean30")))
    features_table %<>% mutate("{col}.d_mean_1v7"     := !!as.name(str_glue("{col}"))          - !!as.name(str_glue("{col}.rmean7")))
    features_table %<>% mutate("{col}.d_mean_3v14"    := !!as.name(str_glue("{col}.rmean3"))   - !!as.name(str_glue("{col}.rmean14")))
    features_table %<>% mutate("{col}.d_mean_7v30"    := !!as.name(str_glue("{col}.rmean7"))   - !!as.name(str_glue("{col}.rmean30")))
    features_table %<>% mutate("{col}.d_mean_3v30"    := !!as.name(str_glue("{col}.rmean3"))   - !!as.name(str_glue("{col}.rmean30")))
    
    
    # Abs of deviation from recent baseline
    
    if(col_class == "numeric"){
      features_table %<>% mutate("{col}.d_mean_1v3.abs"     := abs(!!as.name(str_glue("{col}.d_mean_1v3"))))
      features_table %<>% mutate("{col}.d_mean_3v7.abs"     := abs(!!as.name(str_glue("{col}.d_mean_3v7"))))
      features_table %<>% mutate("{col}.d_mean_7v14.abs"    := abs(!!as.name(str_glue("{col}.d_mean_7v14"))))
      features_table %<>% mutate("{col}.d_mean_14v30.abs"   := abs(!!as.name(str_glue("{col}.d_mean_14v30"))))
      features_table %<>% mutate("{col}.d_mean_1v7.abs"     := abs(!!as.name(str_glue("{col}.d_mean_1v7"))))
      features_table %<>% mutate("{col}.d_mean_3v14.abs"    := abs(!!as.name(str_glue("{col}.d_mean_3v14"))))
      features_table %<>% mutate("{col}.d_mean_7v30.abs"    := abs(!!as.name(str_glue("{col}.d_mean_7v30"))))
      features_table %<>% mutate("{col}.d_mean_3v30.abs"    := abs(!!as.name(str_glue("{col}.d_mean_3v30"))))
    }
    
  }
  
  return(features_table)
}
