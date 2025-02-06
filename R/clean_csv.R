#' Clean raw csv data files
#'
#' @description
#' 'clean_csv_files' cleans up the values from columns with various problems that
#' make them difficult to handle. For example, ID codes in parentheses mixed
#' with character strings or dirty characters such as '/' or unnesesary
#' empty spaces. 'clean_csv_files' also selects relevant columns and sets
#' class for variables
#'
#' @details
#' `clean_csv_files` takes as input a list of paths to files that need to be
#' cleaned. It returns a clean `data.table` per file saved as an RDS file. In
#' addition to this, the function also outputs a log `data.table` with summary
#' information of the files cleaned. Finally, rows with timestamps outside the
#' date of the file are logged to a dt. The file's locations are as follow:
#'
#' Input files directory: `data/raw_data/reportes_dia/`
#' Output files directory: `data/processed_data/val_dia/`
#' Log files directory: `data/aux_data/cleaning_logs/`
#' lost timestamps dir: `data/processed_data/lost_and_found_timestamps/`
#'
#' @param path Path to a csv raw file directory: `data/raw_data/reportes_dia/`
#'
#' @return
#' NULL
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'  clean_csv_files(file)
#' }
#'
clean_csv_files <- function(path) {
  if (file.exists("data/aux_data/cleaning_logs/log_clean.csv")) {
    cache <- fread("data/aux_data/cleaning_logs/log_clean.csv")
    cleaned_files <- cache[, unique(file_name)]

    if (basename(path) %in% cleaned_files) {
      cat("File ", basename(path), "already cleaned\n")
    return(NULL)
    }
  }



  started_at <- proc.time()

  dt <- load_csv(path)

  cat("Loaded file:", basename(path), format(object.size(dt), units = "Mb"), "\n")

  bind_logs(dt, path) # Get the log

  clean_cols(dt) # Modified in place, no need to reassign

  save_lost_rows(dt) # dt not modified, no need to reassign

  save_dt(dt, path)

  cat("Finished file:", basename(path), timetaken(started_at), "\n")

  return(NULL)
}

#' Prepare raw data files for cleaning
#'
#' `load_csv` reads a csv file with raw data and selects nine columns from it.
#' It returns a `data.table` ready for cleanning.
#'
#' @param path Path to raw data file to clean.
#' @return a `data.table` with the relevant columns.
#'
load_csv <- function(path) {
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

#' Logs for raw csv files, original reference
bind_logs <- function(dt, path) {
  log <- dt[, .(validaciones  = .N,
                n_timestamp   = uniqueN(as.IDate(timestamp)),
                n_operador    = uniqueN(operador),
                n_linea       = uniqueN(linea),
                n_ruta        = uniqueN(ruta),
                n_bus         = uniqueN(bus),
                n_dispositvo  = uniqueN(dispositivo),
                n_tarjeta     = uniqueN(tarjeta)
                )][, `:=`(date_cleaned = Sys.Date(),
                          file_name = basename(path))]

  setcolorder(log, "file_name", before="validaciones")

    # Save log file
  log_file <- "data/aux_data/cleaning_logs/log_clean.csv"
  fwrite(log, file = log_file, append = TRUE, compress = "none")

}

#' Clean columns linea, ruta, parada and operador
clean_cols <- function(dt) {
  regex <- "\\(.*\\) +|^\\(.*\\)$|/"
  c_rgx <- "([0-9]{3}[A-Z][0-9]{2}).*"
  m_rgx <- ".*(Portal|Estaci.*) ([[:alnum:]]+) .*"

  dt[, `:=`(
    linea = {
      l <- gsub(regex, "", linea)
      l <- gsub("(\\w) \\w.*", "\\1", l)
      l <- gsub("^$", NA, l)
    },
    ruta = {
      r <- gsub(regex, "", ruta)
      r <- gsub("(.*)[ _][0-9]{8}[ _](.*)", "\\1_\\2", r)
      r <- gsub("^$", NA, r)
    },
    parada = {
      ep <- gsub(regex, "", parada)
      ep <- gsub(c_rgx, "\\1", ep)
      ep <- gsub(m_rgx, "\\1 \\2", ep)
      ep <- gsub("^$", NA, ep)
    },
    operador = gsub(regex, "", operador)
  )]
}


#' Extract and save rows with timestamps dates not equal to column `fecha`
#'
#' `save_lost_rows` extracts the misplaced raws and place them
#' in a data.table to be later retrived by function "find_lost_timestamps". The
#' returned data table is saved to:
#'    "data/processed_data/lost_and_found_timestamps/"
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

  f_name <- "data/processed_data/lost_and_found_timestamps/lost_timestamps.csv"
  fwrite(misplaced_rows, file = f_name, na = NA, append = TRUE)

}


#' Save the clean `data.table` to an RDS file
#'
#' @details
#' Extract the exact name of the original raw data file and use it to name
#' the new `data.table`.
#'
#' Output file directory: `data/processed_data/val_dia/`.
#'
#' @param dt A `data.table`
#' @param path Path to original raw data file
#'
save_dt <- function(dt, path) {
  ext <- gsub("\\..*", ".rds", basename(path))
  o_dir <- "data/processed_data/val_dia/"
  o_file <- paste(o_dir, ext, sep = "")
  saveRDS(dt, file = o_file)
}
