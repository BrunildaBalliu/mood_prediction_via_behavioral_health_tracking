get_unique_deviceids <- function(sqlite_db, table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)

  db_table <- tbl(con, table)
  
  # Extract ordered list of unique device_id from table
  unique_device_ids <- db_table %>% 
    select(device_id) %>%
    distinct %>%
    arrange(device_id)

  # Force lazy query to execute and convert to tibble
  unique_device_ids %<>% 
    collect() %>%
    as_tibble()
  
  DBI::dbDisconnect(con)

  return(unique_device_ids)
}
