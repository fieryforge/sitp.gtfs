#' Load raw data files
#'
#' `load_raw_data` reads a csv raw data file and selects nine columns from it.
#' It returns a `data.table` ready for cleanning.
#'
#' @param path Path to raw data file to clean.
#' @param select.cols Vector of column names to select from raw data file.
#' @param dt.col.names Vector of column names to assign to the returned data table.
#'
#' @return a `data.table` with the relevant columns.
#'
load_raw_data <- function(path, select.cols = NULL, dt.col.names = NULL) {
  if (!file.exists(path))
    stop(paste("Required input file does not exist: ", path))

  # Select and set types for variables
  if (is.null(select.cols)) {
      select.cols <- list("IDate" = "Fecha_Clearing",
                          "character" = c("Linea", "Ruta", "Estacion_Parada",
                                          "Dispositivo", "ID_Vehiculo",
                                          "Operador", "Numero_Tarjeta"),
                          "POSIXct" = "Fecha_Transaccion"
                          )

      dt.col.names <- c("fecha", "linea", "ruta", "parada", "dispositivo", "bus",
                        "operador", "tarjeta", "timestamp")
  }

  dt <- fread(file = path, select = select.cols, col.names = dt.col.names, showProgress = FALSE)

  return(dt)
}


#' Clean parada column from raw data files
#'
#' `clean_parada` takes as input a character vector with unique values from
#' a raw data file's column parada. It returns a data table with colums to
#' be join to the data table being processed. Column `parada` replaces with
#' clean names the column with the same name in the dt being processed.
#'
#' @param parada A character vector with unique values from column `parada` of
#' the data table being processed
#'
clean_parada <- function(parada) {
  dt <- data.table(parada_raw = parada)

  id_rgx <- "^(\\([0-9]+\\))[[:space:]]?.*"
  cenefa_rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*"
  nn_rgx <- "^(.*[)]\\s)(.*)\\|[^|].*" ## exclude second | at the end
  dt[, cenefa := gsub(cenefa_rgx, "\\1", parada_raw)
     ][!grepl(cenefa_rgx, cenefa), cenefa := gsub(nn_rgx, "\\2", cenefa)]

  ## join index
  dt <- index_paradas[dt, on = "cenefa"]

  ## replace id.parada with id from parada raw
  dt[, id.parada := gsub(id_rgx, "\\1", parada_raw)]

  ## get a clean name for rows with cenefas not found in index
  m_rgx <- "[^\\|].*\\|[0-9]{3}[A-Z][0-9]{2}[ _](.*)"
  dt[, parada := ifelse(is.na(parada), gsub(m_rgx, "\\1", parada_raw), parada)]

  ## clean unrecognized char
  dt[grepl("\uFFFD", parada), parada := sapply(parada, replace_error)]

  ## check for new character error and upgrade dict_error
  bad_word <- dt[grepl("\uFFFD", parada), .(parada_bad = parada_raw)]
  fwrite(bad_word, file = "data-raw/dict_error_new_words.csv", append = TRUE, sep = ",")

  ## set the key for the join with DT
  setkey(dt, parada_raw)

  return(dt)
}

## replaces words with unrecognized charater with a valid one
replace_error <- function(error) {

  ## this function must be called by Reduce function
  replace_bad_char <- function(error, word) {
    gsub(word, dict_error[[word]], error)
  }

  good_word <- Reduce(replace_bad_char, names(dict_error), init = error)

  return(good_word)
}

clean_ruta <- function(ruta = NULL) {
  idx <- copy(index_rutas)

  dt <- data.table(ruta_orig = ruta)

  dt <- idx[dt, on = "ruta_orig"]

  no_match <- dt[is.na(ruta)] ## rutas not in index
  no_match[, id := gsub("^(\\(.*\\)) +.*", "\\1", ruta_orig)]
  no_match[, rta.nombre := gsub("^(\\(.*\\)) +(.*)", "\\2", ruta_orig)
           ][, rta.nombre := gsub(" +", "_", rta.nombre)]

  ## new_ruta <- clean_nomatch(no_match, idx)

  ## add new sufix
  dt <- new_ruta[dt, on = "ruta_orig"]
  dt[is.na(i.id), `:=`(i.ruta = ruta, i.id = id)]
  dt[!sapply(sufix.lst, is.null), i.sufix.lst := lapply(seq_along(sufix.lst),
                                                        \(i) sufix.lst[[i]])]

  ## sufix to character class, not list
  dt[, sufix.rta := sapply(i.sufix.lst, paste0, collapse = "|")]
  dt[grepl("^$", sufix.rta), sufix.rta := NA]

  dt[, c("ruta", "sufix.lst", "id", "i.sufix.lst") := NULL]
  setnames(dt, old = c("ruta_orig", "i.ruta", "i.id"),
           new = c("ruta", "ruta.cln", "id.rta"))
  setkey(dt, ruta)

  return(dt)
}

clean_nomatch <- function(dt, idx) {
  dt[, ruta_raw := ruta_orig]

  dt[, ruta_raw := gsub("[[:space:]]{2,}", " ", ruta_raw)]   ## remove multiple spaces

  id_rgx <- "^(\\(.*\\)) (.*)" ## split dt columns into ruta_raw, id, ruta
  dt[, id := gsub(id_rgx, "\\1", ruta_raw)]
  dt[, ruta := gsub("(\\(.*\\) )([^_ ]+)([_ ].*|$)", "\\2", ruta_raw)]
  dt[, ruta_raw := NULL]

  dt <- idx[dt, on = "ruta"]
  dt <- dt[!duplicated(i.id)]
  dt[, c("ruta_orig", "id", "i.sufix.lst") := NULL]
  setnames(dt, old = c("i.ruta_orig", "i.id"), new = c("ruta_orig", "id"))

  return(dt)
}

impute_rutas_by_paradas <- function(dt) {
  rta_to_impute <- dt[is.na(sufix.rta), unique(ruta)]
  rta_cnfa <- dt[, .(cnfa = list(unique(cenefa))), by = ruta]

}
#' Clean columns `ruta` or `linea` from raw data files
#'
#' `clean_col` takes as input a character vector with unique values from
#' a raw data file's columns `ruta` or `linea`. It returns a data table with
#' colums to be join to the data table being processed. Column `col` replaces
#' with clean names the correspondent column in the dt being processed.
#'
#' @param col A character vector with unique values from columns either `ruta`
#' or `linea` of the data table being processed.
#'
clean_column <- function(col = NULL) {
  dt <- data.table(col_raw = col)

  ## strip col_raw into id, colum, col_clean
  id_rgx <- "^(.*[)]{1}) ?(.*)"
  trid_rgx <- "^ *([[:alnum:]-]+|^5_2)[ _]?.*" ## 5_2 special case
  dt[, `:=`(id = gsub(id_rgx, "\\1", col_raw),
            column = gsub(id_rgx, "\\2", col_raw))] ## needed to get clean name
  dt[, col_clean := gsub(trid_rgx, "\\1", column)] #solo nombre para

  ## case codes in  parentesis with no data, inputed by id and get a name
  dt[, col_clean := ifelse(.N > 1,
                           col_clean[which.max(nchar(col_clean))],
                           col_clean), by = id]

  ## take care of wierd codes in parentesis with no data,
  ## inpute name with id, not NA, keep them seperate
  dt[, col_clean := ifelse(col_clean == "", id, col_clean)]

  ## case duplicated col_clean implies multiple versions of the same rta or lna,
  ## add suffix to col_clean
  ##dt[, col_clean := assign_version(dt[, .(col_clean)])]

  ## set the key for the join with DT
  setkey(dt, "col_raw")

  return(dt[, .(col_raw, id, col_clean)])
}

#' Assing a suffix to rutas with the same name but different journeys
#'
#' This function is called by `clean_column` function and it takes as input
#' a data table with clean names, posibly duplicated. It outputs
#' a vector with names with a suffix for duplicated values.
#' base name.
#'
#' @param dt Data table with split names into columns
#'
assign_version <- function(dt) {
  ## find recorridos by suffix
  dt[, suffix_grp := .GRP, by = col_clean
     ][, col_clean := if (.N > 1) {
                        paste(col_clean, letters[seq_len(.N)], sep = "_")
                      } else {
                        col_clean
                      },
       by = suffix_grp]

  return(dt[, col_clean])
}

#' Clean `operador` column from raw data files
#'
#' `clean_operador` takes as input a character vector with unique values from
#' a raw data file's column `operador`. It returns a data table with colums to
#' be join to the data table being processed. Column `op_raw` replaces with
#' clean names the column with the same name in the dt being processed.
#'
#' @param operador A character vector with unique values from column `operador`
#'
clean_operador <- function(operador) {
  dt <- data.table(op_raw = operador)

  rgx <- "^(.*[)]{1}) ?(.*)"
  dt[, `:=`(id.operador = gsub(rgx, "\\1", operador),
            operador = sapply(operador, unify_operador))]
            #operador = gsub(rgx, "\\2", operador))]

  ## set the key for the join with DT
  setkey(dt, "op_raw")

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

  f_name <- "data-raw/lost_timestamps.csv"
  fwrite(misplaced_rows, file = f_name, na = NA, append = TRUE)

}

#' Save clean data table
#'
#' Save clean data table to directory: `../data_sets/sitp/clean/`
#'
#' @param dt A data table
#' @param path Path with the original raw file name
save_clean_dt <- function(dt, path) {
  ## get file name from path
  f_name <- gsub("\\.zip", "\\.rds", basename(path))
  f_name <- paste0("../data_sets/sitp/clean/", f_name)

  saveRDS(dt, f_name)

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

  dt <- extract_misplaced_rows(dt, date)

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
extract_misplaced_rows <- function(dt, date) {

  # Prepare the valid timestamp range
  start <- as.POSIXct(date, tz = "UTC", time = 0)
  end <- as.POSIXct(date, tz = "UTC", time = 86399)

  # Create dt with timestamps in range
  dt <- dt[inrange(timestamp, lower = start, upper = end)]

  return(dt)
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
  file <- "data-raw/lost_timestamps.csv"
  lost_timestamps <- fread(file)

  # Find lost rows to bind
  dt <- lost_timestamps[inrange(timestamp, lower = start, upper = end)]

  return(dt)
}

unify_operador <- function(operador) {
  if (grepl("EMASIVO", operador)) "EMASIVO"
  else if (grepl("ETIB", operador)) "ETIB"
  else if (grepl("SUMA", operador)) "SUMA"
  else if (grepl("GMOVIL", operador)) "GMOVIL"
  else if (grepl("MASIVO CAPITAL", operador)) "MASIVO CAPITAL"
  else if (grepl("AM\u00C9RICAS|AMERICAS", operador)) "GRAN AM\u00C9RICAS"
  else if (grepl("E-SOMOS", operador)) "E-SOMOS"
  else if (grepl("ESTE ES MI BUS", operador)) "ESTE ES MI BUS"
  else if (grepl("CONSORCIO EXPRESS", operador)) "CONSORCIO EXPRESS"
  else if (grepl("ZMO", operador)) "ZMO"
  else if (grepl("MUEVE", operador)) "MUEVE"
  else if (grepl("OPERADORA DISTRITAL", operador)) "OPERADORA DISTRITAL"
  else if (grepl("RECAUDO|Recaudo", operador)) "RECAUDO"
  else operador
}
