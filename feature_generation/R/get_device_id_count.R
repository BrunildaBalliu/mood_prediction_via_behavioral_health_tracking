get_device_id_count <- function(sqlite_db, 
                                table,
                                device_id) {
  
  devicestr <- device_id %>%
    simplify() %>%
    first()
  
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  
  db_table <- tbl(con, table)
  
  # Count records from the target device id
  device_id_count <- db_table %>% 
    dplyr::filter(device_id == devicestr) %>%
    tally()
  
  # Force lazy query to execute and convert to tibble
  device_id_count <- device_id_count %>%
    collect() %>%
    as_tibble()
  
  print(str_glue("{device_id_count} records found for device id {device_id}"))
  
  DBI::dbDisconnect(con)
  
  result = cbind(device_id,device_id_count)
  return(result)
}
