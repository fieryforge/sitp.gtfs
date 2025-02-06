#' Extract misplaced rows with unmatched timestamp in column Fecha_Transaccion
#'
#' `extract_misplaced_rows` returns a data table with all records with
#' timestamps in column Fecha_Transaccion that do not match the date in
#' column Fecha_Clearing
#'
#' The returned data table is saved to:
#'  "data/processed_data/wrong_f_transaccion/"
#'
#' @param rds_dt An rds data.table file from "data/processed_data/val_dia/"
#' @return A data.table
#'
xtract_misplaced_rows <- function(rds_dt) {
  dt <- readRDS(rds_dt)
  # Assign from file's name
  date <- ".*([0-9]{4})([0-9]{2})([0-9]{2}).*"
  f_clearing <- as.IDate(gsub(pattern = date, "\\1-\\2-\\3", rds_dt))

  # Prepare the valid timestamp range
  start <- as.POSIXct(f_clearing, tz = "UTC", time = 0)
  end <- as.POSIXct(f_clearing, tz = "UTC", time = 86399)

  # Create dt with out of range timestamps
  misplaced_timestamps <- dt[!inrange(Fecha_Transaccion,
                                      lower = start, upper = end)]
  f_name <- paste0("misplaced_Fecha_Transaccion_",
                   as.character(f_clearing), ".rds")

  saveRDS(misplaced_timestamps,
          paste0("data/processed_data/lost_and_found_timestamps/", f_name))
}

#' Find and bind lost rows by timestamps in col Fecha_Transaccion
#'
#' `bind_lost_rows` binds rows from misplaced rows in data files where column
#' Fecha_Transaccion does not match the date in col Fecha_Clearing. It returns a
#' data.table with all rows found that match their timestamps with the
#' file's date
#'
#' @param dt A data.table file from "data/processed_data/val_dia/"
#' @return A data.table
#'
find_lost_timestamps <- function(dt) {
  fecha <- dt[1, fecha]

  start <- as.POSIXct(fecha, tz = "UTC", time = 0)
  end <- as.POSIXct(fecha, tz = "UTC", time = 86399)

  # Remove misplaced rows
  dt <- dt[inrange(timestamp, lower = start, upper = end)]

  # read lost_timestamps.csv file
  file <- "data/processed_data/lost_and_found_timestamps/lost_timestamps.csv"
  lost_timestamps <- fread(file)

  # Find lost rows to bind
  rows_found <- lost_timestamps[inrange(timestamp, lower = start, upper = end)]

  # Bind the found rows
  dt <- rbindlist(list(dt, rows_found))

  return(dt)
}
