#' Clean raw csv data files
#'
#' @description
#' 'clean_csv_files' cleans up the values from columns with various problems that
#' make them difficult to handle. For example, ID codes in parentheses mixed
#' with character strings or dirty characters such as '/', '.' or unnesesary
#' empty spaces. 'clean_csv_files' also does a selection of columns leaving out
#' others from the original csv file which are irrelevant for this package.
#'
#' @details
#' `clean_csv_files` takes as input a list of paths to files that need to be
#' cleaned. It returns a clean `data.table` per file saved as an RDS file. In
#' addition to this, the function also outputs a log `data.table` with summary
#' information of the files cleaned, the file's locations are as follow:
#'
#' Input files directory: `data/raw_data/reportes_dia/`
#' Output files directory: `data/processed_data/val_dia/`
#' Log files directory: `data/aux_data/cleaning_logs/`
#'
#' @param files path to a file or a list of files in directory:
#' `data/raw_data/reportes_dia/`
#'
#' @return
#' NULL
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'  clean_csv_files(files)
#' }
#'
clean_csv_files <- function(files) {
  # List of data tables to bind for the log file
  logs_list <-
    lapply(files, function(f) { # The main function
      started_at <- proc.time()
      dt <- load_csv(f)
      cat("Loaded file:", basename(f), format(object.size(dt), units = "Mb"), "\n")
      clean_cols(dt)
      save_dt(dt, f)
      cat("Finished file:", basename(f), timetaken(started_at), "\n")
      log <- bind_logs(dt, f) # Get the log
    })

  # Save logs file
  t <- format(Sys.time(), "%Y%m%d_%H:%M:%S")
  log_file <- paste0("data/aux_data/cleaning_logs/", "clean_log-", t, ".rds")
  log_dt <- rbindlist(logs_list)
  saveRDS(log_dt, file = log_file)

  return(NULL)
}

#' Prepare raw data files for cleaning
#'
#' `load_csv` reads a csv file with raw data and selects nine columns from it.
#' It returns a `data.table` ready for cleanning.
#'
#' @param file Path to raw data file to clean.
#' @return a `data.table` with the relevant columns.
#' 
load_csv <- function(file) {
  if (!file.exists(file))
    stop(paste("Required input file does not exist: ", file))

  # Set types for variables
  selected_cols <- c("Fecha_Clearing" = "IDate",
                     "Fecha_Transaccion" = "POSIXct",
                     "Numero_Tarjeta" = "character",
                     "Dispositivo" = "integer",
                     "ID_Vehiculo" = "integer",
                     "Linea" = "character",
                     "Ruta" = "character",
                     "Estacion_Parada" = "character",
                     "Operador" = "character")

  dt <- fread(file = file, select = selected_cols, showProgress = FALSE)
}

clean_cols <- function(dt) {
  regex <- "\\(.*\\) +|^\\(.*\\)$"
  m_rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*|.*(Portal.*)|.*(Estac.*)"

  dt[, `:=`(
    Linea = {
      l <- sub(regex, "", Linea)
      l <- sub("(\\w) \\w.*|[/]", "\\1", l)
      l <- sub("^$", NA, l)
    },
    Ruta = {
      r <- sub(regex, "", Ruta)
      r <- sub("(.*)[ _][0-9]{8}[ _](.*)", "\\1_\\2", r)
      r <- sub("^$", NA, r)
    },
    Estacion_Parada = {
      ep <- sub(regex, "", Estacion_Parada)
      ep <- sub(m_rgx, "\\1\\2\\3", ep)
      ep <- sub("^$", NA, ep)
    },
    Operador = sub(regex, "", Operador)
  )]
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
#' @param f_name Path to original raw data file
#'
save_dt <- function(dt, f_name) {
  f_noext <- gsub("\\..*", "", basename(f_name))
  o_dir <- "data/processed_data/val_dia/"
  o_file <- paste(f_noext, ".rds", sep = "")
  saveRDS(dt, file = paste(o_dir, o_file, sep = ""))
}

#' Write logs of the cleaning for each file prossesed
#'
#' @param dt A `data.table`.
#' @param file_name A path to original file name.
#'
bind_logs <- function(dt, file_name) {
  log <- data.table(file_name = basename(file_name),
                    Fecha_Clearing = unique(dt$Fecha_Clearing),
                    Fechas_Transaccion = list(sort(unique(as.IDate(dt$Fecha_Transaccion)))),
                    Operador = uniqueN(dt$Operador),
                    Validaciones = nrow(dt),
                    Linea = uniqueN(dt$Linea),
                    Linea_na = dt[is.na(Linea), .N],
                    Ruta = uniqueN(dt$Ruta),
                    Ruta_na = dt[is.na(Ruta), .N],
                    Vehiculos = uniqueN(dt$ID_Vehiculo),
                    Dispositivos = uniqueN(dt$Dispositivo),
                    total_tarjetas = uniqueN(dt$Numero_Tarjeta))
}
