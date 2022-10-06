process_config <- function(config) {
  datetime = config$freeze_datetime
  config$freeze_string = paste0(
    year(datetime),
    month(datetime),
    day(datetime)
  )
  # Database uses timestamps in UTC time zone
  attributes(datetime)$tzone <- "UTC"
  config$freeze_datetime_db = as.character(datetime)
  # AWARE uses Unix timestamp to millisecond resolution
  config$freeze_timestamp_aware = as.numeric(datetime)*1000
  
  return(config)
}
