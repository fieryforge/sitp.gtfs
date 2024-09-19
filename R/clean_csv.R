#' Clean raw csv data files
#'
#' @description
#' 'clean_csv_files' cleans up the values from columns with various problems that
#' make them difficult to handle. For example, ID codes in parentheses mixed
#' with character strings or dirty characters such as '/', '.' or unnesesary
#' empty spaces. 'clean_csv_files' also does a selection of columns leaving out
#' others that the original csv file contains which are irrelevant to this
#' package.
#'
#' `clean_csv_files` returns a `data.table` with clean data.
#'
#' @details `clean_csv_files` takes as input a list of files for cleanning. This
#' function calls eight different functions in succession, each performing a
#' different task:
#'
#' 1. `load_csv` reads the file or list of files from @param file and then
#' selects nine columns from each file. It returns a `data.table` ready
#' for cleanning.
#'
#' 2. `cl_parentesis` splits columns with values that have two parts: an ID
#' with numbers in parenthesis and a string. It creates a new column for
#' the ID code and leaves a clean string in the original column.
#'
#' 3. `cl_lre` cleans columns Linea, Ruta and Estacion_Parada extracting
#' various ID codes.
#'
#' 4. `empty2na` empty values produced by previous cleaning are replaced with
#' NA's.
#'
#' 5. `impute_linea` tries to impute ande replace NA's value based on other
#' values relevant for variable Linea.
#'
#' 6. `impute_ruta` does the same as `impute_linea` but for variable Ruta.
#'
#' 7. `save_dt` saves the clean `data.table` to an RDS file in directory:
#' Output file -> [data/processed_data/val_dia/validacionZonalYYYYMMDD.rds]
#'
#' 8. `print_log` cat relevant information from the cleaning process to a
#' log file.
#'
#' @param files path to a file or a list of files in directory:
#' `data/raw_data/reportes_dia/`
#'
#' @return
#' A `data.table` saved to a file named as the original file in format RDS to
#' directory: `data/processed_data/val_dia/`.
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'  clean_csv_files(files)
#' }
#'
clean_csv_files <- function(files) {
  logs_list <-
    lapply(files, function(f) {
      started_at <- proc.time()
      dt <- load_csv(f)
      cat("Loaded file:", basename(f), format(object.size(dt), units = "Mb"), "\n")
      cols_no_parens(dt)
      clean_cols(dt)
      empty2na(dt)
      catch_day(dt)
      save_dt(dt, f)
      cat("Finished file:", basename(f), timetaken(started_at), "\n")
      log <- bind_logs(dt, f)
    })

  t <- format(Sys.time(), "%Y%m%d_%H:%M:%S")
  log_file <- paste0("data/aux_data/cleaning_logs/", "clean_log-", t, ".rds")
  log_dt <- rbindlist(logs_list)
  saveRDS(log_dt, file = log_file)
}

## Helper functions for clean_csv.R script

load_csv <- function(file) {
  input <- ifelse(file.exists(file), file,
                  stop(paste("File not found: ", file)))

  selected_cols <- c("Fecha_Clearing"="IDate",
                     "Fecha_Transaccion"="POSIXct",
                     "Numero_Tarjeta"="character",
                     "Dispositivo"="character",
                     "ID_Vehiculo"="character",
                     "Linea"="character",
                     "Ruta"="character",
                     "Estacion_Parada"="character",
                     "Operador"="character")

  dt <- fread(input, select = selected_cols, showProgress = FALSE)
  setnames(dt, c("Fecha_Clearing"="fecha_clearing",
                 "Fecha_Transaccion"="fecha_transaccion",
                 "Numero_Tarjeta"="tarjeta",
                 "Dispositivo"="dispositivo",
                 "ID_Vehiculo"="id_vehiculo",
                 "Linea"="linea",
                 "Ruta"="ruta",
                 "Estacion_Parada"="parada",
                 "Operador"="operador"))
}

cols_no_parens <- function(dt) {
  cols <- c("linea", "ruta", "parada", "operador")
  regex <- "\\(.*\\) +|^\\(.*\\)$"
  dt[, (cols) := lapply(.SD, function(x) sub(regex, "", x)), .SDcols = cols]
}

clean_cols <- function(dt) {
  m_rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*|.*(Portal.*)|.*(Estac.*)"
  dt[, `:=`(linea = sub("(\\w) \\w.*|[/]", "\\1", linea),
            parada = sub(m_rgx, "\\1\\2\\3", parada))]
}

empty2na <- function(dt) {
  empty_str <- c("linea", "ruta", "parada")
  dt[, (empty_str) := lapply(.SD, \(x) sub("^$", NA, x)),
     .SDcols = empty_str]
}

catch_day <- function(dt) {
  fecha <- dt[, unique(fecha_clearing)]
  if (length(fecha == 1)) {
    fecha
  } else {
    warning("fecha_clearing is not unique.")
    fecha
  }
}

save_dt <- function(dt, f_name) {
  f_noext <- gsub("\\..*", "", basename(f_name))
  o_dir <- "data/processed_data/val_dia/"
  o_file <- paste(f_noext, ".rds", sep = "")
  saveRDS(dt, file = paste(o_dir, o_file, sep = ""))
}

bind_logs <- function(dt, file_name) {
  log <- data.table(file_name = basename(file_name),
                    fecha_clearing = catch_day(dt),
                    fechas_transaccion = list(sort(unique(as.IDate(dt$fecha_transaccion)))),
                    operadores = uniqueN(dt$operador),
                    validaciones = nrow(dt),
                    lineas = uniqueN(dt$linea),
                    linea_na = dt[is.na(linea), .N],
                    rutas = uniqueN(dt$ruta),
                    rutas_na = dt[is.na(ruta), .N],
                    vehiculos = uniqueN(dt$id_vehiculo),
                    dispositivos = uniqueN(dt$dispositivo),
                    total_tarjetas = uniqueN(dt$tarjeta))
}
