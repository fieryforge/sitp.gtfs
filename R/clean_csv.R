#' Clean raw csv data files
#'
#' @description
#' 'clean_csv_files' cleans up the values of columns with various problems that
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
  ## if (file.exists("data/aux_data/cleaning_logs/log_clean.csv")) {
  ##   cache <- fread("data/aux_data/cleaning_logs/log_clean.csv")
  ##   cleaned_files <- cache[, unique(file_name)]

  ##   if (basename(path) %in% cleaned_files) {
  ##     cat("File ", basename(path), "already cleaned\n")
  ##   return(NULL)
  ##   }
  ## }

  started_at <- proc.time()

  dt <- load_csv(path)

  cat("Loaded file:", basename(path), format(object.size(dt), units = "Mb"), "\n")

  ##bind_logs(dt, path) # Get the log

  dt <- clean_column(dt, col = "ruta")

  dt <- clean_column(dt, col = "linea")

  dt <- clean_parada(dt)

  ## save_lost_rows(dt) # dt not modified, no need to reassign

  ## save_dt(dt, path)

  dt[, `:=`(linea.1 = NULL, ruta.1 = NULL)]

  cat("Finished file:", basename(path), timetaken(started_at), "\n")

  return(dt)
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
  log_file <- "../sitp/data/aux_data/cleaning_logs/log_clean.csv"
  fwrite(log, file = log_file, append = TRUE, compress = "none")

}


clean_column <- function(dt, col = NULL) {
  ## creat column id.col in dt, the col to join on
  id_rgx <- "^(\\(.*\\)) ?.*"
  dt[, id := gsub(id_rgx, "\\1", dt[[col]])]

  idx <- strip_raw_col(dt, col = col)
  idx <- idx[, .(id, col_clean)]

  ## where to make the join
  setkey(idx, id)
  setkey(dt, id)
  dt <- idx[dt]

  ## names according to arg col
  setnames(dt, old = c("id", "col_clean"), new = c(paste0("id.", col), col))

  ## ## TODO before ran in scale save to file to build an index of rutas
  ## if (col == "ruta") {
  ##   fecha <- dt[1, fecha]
  ##   file <- paste0("../sitp/data/aux_data/rutas/dict_rutas-", fecha, ".csv")
  ##   fwrite(idx, file, append = TRUE )
  ## }

  return(dt)
}

clean_ruta <- function(dt) {
  idx <- strip_raw_col(dt, col = "ruta")
  idx <- idx[, .(col_raw, id, col_clean)]

  setkey(idx, col_raw)
  setkey(dt, ruta)
  dt <- idx[dt]
  dt[, col_raw := NULL]
  setnames(dt, old = c("id", "col_clean"), new = c("id.ruta", "ruta"))

  ## save to file to build an index
  fecha <- dt[1, fecha]
  file <- paste0("../sitp/data/aux_data/rutas/dict_rutas-", fecha, ".csv")
  fwrite(idx, file, append = TRUE )

  return(dt)
}


#' Clean parada column on raw dt and clean names and id
clean_parada <- function(dt) {
  index_paradas <- fread("../sitp/data/aux_data/paradas/index_paradas.csv")

  ## create col id.parada in DT, where the join will take place
  id_rgx <- "^(\\([0-9]{5}\\)) ?.*"
  dt[, id.parada := gsub(id_rgx, "\\1", parada)]

  ## work on an isolated dt to do the cleannig
  dt_cl <- dt[, unique(.SD), .SDcols = c("id.parada", "parada")]

  cenefa_rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*"
  nn_rgx <- "^(\\([0-9]{5}\\) )(.*)\\|.*" ## TODO there could be any num of digits in ()
  dt_cl[, cenefa := gsub(cenefa_rgx, "\\1", parada)
        ][!grepl(cenefa_rgx, cenefa), cenefa := gsub(nn_rgx, "\\2", cenefa)]

  ## fix names for cenefas not found on pz (eg. rows with col nombre == NA
  m_rgx <- "[^\\|].*\\|([^\\|].*)"
  dt_cl[, parada := (gsub(m_rgx, "\\1", parada))]

  ## clean cenefa strubing from Estacion_Parada
  p_rgx <- "[0-9]{3}[A-Z][0-9]{2}[_ \\s]"
  dt_cl[, parada := (gsub(p_rgx, "", parada))]

  ## join index on cenefa
  dt_cl <- index_paradas[dt_cl, on = "cenefa"]
  dt_cl[is.na(parada), parada := i.parada
        ][, `:=`(id.parada = NULL, i.parada = NULL)]
  
  setnames(dt_cl, "i.id.parada", "id.parada")

  ## join with dt
  dt <- dt_cl[dt, on = "id.parada"]
  dt[, i.parada := NULL]

  ## try clean \uFFFD char
  dt[grepl("\uFFFD", parada), parada := sapply(parada, replace_error)]
  ##  grab new words with bad char to update dict
  np_char <- dt[grepl("\uFFFD", parada), .(np_char = unique(parada)) ]
  fwrite(np_char, "../sitp/data/aux_data/paradas/update_dict.csv",
         append = TRUE, sep = ",")

  setcolorder(dt, c("id.parada", "parada"), before = c("cenefa"))

  return(dt)

}

strip_raw_col <- function(dt, col = NULL) {
  col <- dt[[col]]
  cols <- dt[, .(col_raw = unique(col))]

  id_rgx <- "(\\(.*\\))[ _]+?(.*)"
  trid_rgx <- "^ *([[:alnum:]-]+|^5_2)[ _]?.*" ## 5_2 special case from original csv

  ## fix duplicated id codes without a name
  ## TODO this if, will it ever run? Not clear
  d <- cols[, .(id = gsub(id_rgx, "\\1", col_raw), r = col_raw)][duplicated(id), id]
  if (length(d) > 0) {
    cols[, id := gsub(id_rgx, "\\1", col_raw)]
    cols[, col_raw := if (.N > 1) col_raw[which.max(nchar(col_raw))] else col_raw,
          by = id]
    cols <- cols[, unique(.SD)]
  }

  cols[, id := unique(gsub(id_rgx, "\\1", col_raw))
        ][, col := gsub(id_rgx, "\\2", col_raw) #no parentesis
          ][, col_clean := gsub(trid_rgx, "\\1", col) #solo nombre
            ][, names(cols) := lapply(.SD,  #no empty str
                                       \(x) ifelse(x == "", id, x)),]


  ## find recorridos by suffix
  cols[, suffix_grp := .GRP, by = col_clean
        ][, col_clean := if (.N > 1) {
                            paste(col_clean, letters[seq_len(.N)], sep = "_")
                          } else {
                            col_clean},
          by = suffix_grp]

  cols[, suffix_grp := NULL]

  return(cols)
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
