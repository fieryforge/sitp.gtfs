#' Clean raw data files
#'
#' @description
#' 'clean_raw_data' cleans up the files of the main data set.
#'
#' @details
#' `clean_raw_data` takes as input a list of paths to files that need to be
#' cleaned. It writes the clean data to a file and returns a `data.table`. The
#' the function also writes a log `data.table` with summary information of the
#' files cleaned. Finally, rows with timestamps outside the date written on
#' the file's name are saved to the `lost_and_found_timestamps` file.
#'
#' The files locations are as follow:
#'
#' Input files directory: `data/raw_data/reportes_dia/`
#' Output files directory: `data/processed_data/val_dia/`
#' Log files directory: `data/aux_data/cleaning_logs/`
#' lost timestamps dir: `data/processed_data/lost_and_found_timestamps/`
#'
#' @param path Path to raw data files in directory:
#' `data/raw_data/reportes_dia/`
#'
#' @return
#' A data table
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'  clean_raw_data(path)
#' }
## TODO: create dataset buses based on IDs from FlotaViculada y buses en operacion
clean_raw_data <- function(path = NULL) {
  started_at <- proc.time()

  dt <- load_raw_data(path = path)
  cat("Loaded file:", basename(path), format(object.size(dt),
                                             units = "Mb"), "\n")

  cols <- c("operador", "linea", "ruta", "parada")
  cols_unique <- dt[, unlist(lapply(.SD, unique)), .SDcols = cols]

  operador <- cols_unique[grepl("operador", names(cols_unique))]
  dt_op <- clean_operador(operador)
  setkey(dt, "operador")
  dt <- dt_op[dt][, op_raw := NULL]

  linea <- cols_unique[grepl("linea", names(cols_unique))]
  col <- clean_column(linea)
  setkey(dt, "linea")
  dt <- col[dt][, col_raw := NULL]
  setnames(dt, old = c("id", "col_clean"), new = c("id.linea", "linea"))

  ruta <- cols_unique[grepl("ruta", names(cols_unique))]
  col <- clean_column(ruta)
  setkey(dt, "ruta")
  dt <- col[dt][, col_raw := NULL]
  setnames(dt, old = c("id", "col_clean"), new = c("id.ruta", "ruta"))

  parada <- cols_unique[grepl("parada", names(cols_unique))]
  col <- clean_parada(parada)
  setkey(dt, "parada")
  dt <- col[dt][, parada_raw := NULL]

  save_lost_rows(dt) ## take care of misplaced timestamps

  cat("Finished file:", basename(path), timetaken(started_at), "\n")

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
  index_paradas <- fread("../sitp/data/aux_data/paradas/index_paradas.csv")

  dt <- data.table(parada_raw = parada)

  id_rgx <- "^(\\(.*\\))[ ].*"
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

  ## set the key for the join with DT
  setkey(dt, parada_raw)

  return(dt)

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
  dt[, col_clean := assign_version(dt[, .(col_clean)])]

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
            operador = gsub(rgx, "\\2", operador))]

  ## set the key for the join with DT
  setkey(dt, "op_raw")

  return(dt)
}
