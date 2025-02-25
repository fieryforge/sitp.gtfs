#' Load raw data files
#'
#' `load_csv` reads a csv raw data file and selects nine columns from it.
#' It returns a `data.table` ready for cleanning.
#'
#' @param path Path to raw data file to clean.
#' @return a `data.table` with the relevant columns.
#'
load_raw_data <- function(path) {
  if (!file.exists(path))
    stop(paste("Required input file does not exist: ", path))

  # Select and set types for variables
  selected_cols <- c("Fecha_Clearing" = "IDate",
                     "Fecha_Transaccion" = "POSIXct",
                     "Numero_Tarjeta" = "character",
                     "Dispositivo" = "character",
                     "ID_Vehiculo" = "character",
                     "Linea" = "character",
                     "Ruta" = "character",
                     "Estacion_Parada" = "character",
                     "Operador" = "character")

  dt <- fread(file = path, select = selected_cols, showProgress = FALSE)

  # set better names for columns
  old <- names(selected_cols)
  new <- c("fecha", "timestamp", "tarjeta", "dispositivo", "bus",
           "linea", "ruta", "parada", "operador")
  setnames(dt, old, new)

  return(dt)
}

#' Extract and save rows with timestamps dates not equal to column `fecha`
#'
#' @description
#' This function is called by function `clean_raw_data` while cleaning raw data
#' files.
#'
#' It takes a dt as input and extracts rows with misplaced timestamps and writes them
#' to a data.table to be later retrived by function "find_lost_timestamps". The
#' returned data table is saved to:
#'    "../sitp/data/processed_data/lost_and_found_timestamps/"
#'
#' @param dt The data table being processed by the main function
#' @return A data.table
#'
save_lost_rows <- function(dt) {

  fecha <- dt[1, fecha]

  # Prepare the valid timestamp range
  start <- as.POSIXct(fecha, tz = "UTC", time = 0)
  end <- as.POSIXct(fecha, tz = "UTC", time = 86399)

  # Create dt of misplaced rows by timestamps and save it
  misplaced_rows <- dt[!inrange(timestamp,
                                      lower = start, upper = end)]

  f_name <- "../sitp/data/processed_data/lost_and_found_timestamps/lost_timestamps.csv"
  fwrite(misplaced_rows, file = f_name, na = NA, append = TRUE)

}

#' Load clean data files
#'
#' This function takes as input a path to a clean data file. It reads the file
#' and calls fuctions `extract_misplaced_rows` and `find_lost_timestamps`. It returns
#' a data table with all rows from the same date as the file's name. It depends on the
#' file `lost_timestamps.csv` to find the rows of the same date.
#'
#' @param path A path to a clean data file in dir:
#'    `../sitp/data/processed_data/val_dia/`
#'
load_clean_data <- function(path) {
  dt <- readRDS(path)

  date <- dt[1, fecha]

  dt <- extract_misplaced_rows(date)

  found_rows <- find_lost_timestamps(date)

  if (nrow(found_rows) > 0) dt <- rbindlist(list(dt, found_rows))

  return(dt)
}

#' Extract misplaced rows with unmatched timestamp in column fecha
#'
#' This function takes as input a date. It returns
#' a data table with all records with timestamps inside the 24 hour
#' range for that date.
#'
#' @param date The date to create the 24 hour range to select rows
#'
extract_misplaced_rows <- function(date) {

  # Prepare the valid timestamp range
  start <- as.POSIXct(date, tz = "UTC", time = 0)
  end <- as.POSIXct(date, tz = "UTC", time = 86399)

  # Create dt with timestamps in range
  dt <- dt[inrange(timestamp, lower = start, upper = end)]

}

#' Find and bind lost rows by timestamps in col timestamp
#'
#' This function takes as input a date from a clean data file and returns a data
#' table with rows found in file `lost_timestamps.csv` that match the date of the
#' parameter `date`.
#'
#' @param date A date from a clean file `data/processed_data/val_dia/`
#' @return A data.table
#'
find_lost_timestamps <- function(date) {

  start <- as.POSIXct(date, tz = "UTC", time = 0)
  end <- as.POSIXct(date, tz = "UTC", time = 86399)

  # read lost_timestamps.csv file
  file <- "../sitp/data/processed_data/lost_and_found_timestamps/lost_timestamps.csv"
  lost_timestamps <- fread(file)

  # Find lost rows to bind
  dt <- lost_timestamps[inrange(timestamp, lower = start, upper = end)]

  return(dt)
}
