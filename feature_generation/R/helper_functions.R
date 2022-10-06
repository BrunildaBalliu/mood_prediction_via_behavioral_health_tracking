connect_db <- function(target_db) {
  
  require(DBI)
  require(RMySQL)
  
  # Read in credentials from environment
  if(target_db == "AWARE"){
    dbname = Sys.getenv("AWARE.DBNAME")
    host = Sys.getenv("AWARE.HOST")
    port = as.integer(Sys.getenv("AWARE.PORT"))
    user = Sys.getenv("AWARE.USER")
    password = Sys.getenv("AWARE.PASSWORD")
  }
  else if(target_db == "REDCAP"){
    dbname = Sys.getenv("REDCAP.DBNAME")
    host = Sys.getenv("REDCAP.HOST")
    port = as.integer(Sys.getenv("REDCAP.PORT"))
    user = Sys.getenv("REDCAP.USER")
    password = Sys.getenv("REDCAP.PASSWORD")
  }
  else{
    stop(sprintf("No credentials found for target database: %s",target_db))
  }
  
  db_conn <- dbConnect(
    MySQL(),
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  )
  
  return(db_conn)
}

run_query_on_db <- function(target_db, query) {
  db_conn <- connect_db(target_db)
  on.exit(dbDisconnect(db_conn))
  
  result <- dbGetQuery(db_conn, query)
  
  return(result)
}


convert_datetime_dataframe <- function(dataframe, timestamp_name = "timestamp", datetime_name = "datetime") {
  dataframe[[datetime_name]] <-
    convert_timestamp_to_datetime(dataframe[[timestamp_name]])
  return(dataframe)
}

convert_timestamp_to_datetime <- function(timestamp) {
  datetime <- as.POSIXct(timestamp / 1000,
                         origin = "1970-01-01",
                         tz = "America/Los_Angeles",
                         usetz = TRUE)
  return(datetime)
}

distHaversineVect <- function(longs1, lats1, longs2, lats2){
  require("geosphere")
  df=cbind(longs1,lats1,longs2,lats2)
  result=apply(df,1,distHaversineRow)
  return(result)
}

distHaversineRow <- function(geo_row){
  require("geosphere")
  result = distHaversine(c(geo_row[1],geo_row[2]),c(geo_row[3],geo_row[4]))
  return(result)
}

expand_timestamp_metrics <- function(data_table) {
  require(dplyr)
  require(tidyr)
  require(magrittr)
  
  data_table %<>%
    mutate(unix_timestamp = round(timestamp/1000, digits = 0)) %>%
    mutate(POSIXct = convert_timestamp_to_datetime(timestamp)) %>%
    arrange(POSIXct) %>%
    mutate(prev_POSIXct = dplyr::lag(POSIXct),
           next_POSIXct = dplyr::lead(POSIXct)) %>%
    mutate(interval_min = ifelse(
      is.na(prev_POSIXct),
      POSIXct,
      prev_POSIXct + difftime(POSIXct,prev_POSIXct)/2)) %>%
    mutate(interval_max = ifelse(
      is.na(next_POSIXct),
      POSIXct,
      POSIXct + difftime(next_POSIXct,POSIXct)/2)) %>%
    mutate(time_duration_s = interval_max -interval_min,
           time_duration_m = time_duration_s / 60)
  
  data_table %<>% 
    mutate(date = date(POSIXct),
           time_of_day = paste(lubridate::hour(POSIXct),
                               lubridate::minute(POSIXct),
                               as.integer(lubridate::second(POSIXct)),
                               sep=":"), # shows time in timezone of the POSIXct object, not local timezone the way strftime does
           hour_of_day = lubridate::hour(POSIXct),
           is_night = (hour_of_day >= 0 & hour_of_day <6),
           is_workhours = (hour_of_day >= 9 & hour_of_day <17),
           day_of_week = lubridate::wday(date),
           weekday = weekdays(date),
           is_weekend = weekday %in% c("Saturday", "Sunday"))
  
  
  return(data_table)
}


dbgvar <- function(...){
  # tmp <- ...
  mc = match.call(expand.dots = FALSE)
  var_names = as.list(mc[-1]$...)
  var_values = list(...)
  for(i in 1:length(var_values)){
    name = var_names[i]
    value = var_values[i] 
    print(str_glue("{name}\n---------------"))
    print(value)
    cat("\n")
  }
}

merge_features <- function(x, y, by){
  if(is.null(x) || nrow(x)==0){
    result = y
  }else if(is.null(y) || nrow(y)==0){
    result = x
  }
  else{
    result = full_join(x, y, by = by)
  }
  return(result)
}
