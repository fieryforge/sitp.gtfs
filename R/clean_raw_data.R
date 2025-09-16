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
#' @param path Path to raw data files in directory: `data/raw_data/reportes_dia/`.
#' @param select.cols Vector with column names to clean from raw data file.
#' @param dt.col.names Vector with column names to assing to the clean data table.
#' @param build.idx Logical, if TRUE, build the index, don't clean.
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
clean_raw_data <- function(path, select.cols = NULL, dt.col.names = NULL, build.idx = FALSE) {
  started_at <- proc.time()

  dt <- load_raw_data(path = path, select.cols = select.cols,
                      dt.col.names = dt.col.names)

  nrow_pre <- nrow(dt)

  cat("Loaded file:", basename(path), format(object.size(dt),
                                             units = "Mb"), "\n")

  if (is.null(select.cols) & is.null(dt.col.names)) {
    cols <- c("operador", "linea", "ruta", "parada")
  } else {
    cols <- names(dt)
  }

  cols_unique <- dt[, unlist(lapply(.SD, unique)), .SDcols = cols]

  if (build.idx) return(cols_unique) ## build index and stop

  if ("operador" %in% cols) {
    operador <- cols_unique[grepl("operador", names(cols_unique))]
    dt_op <- clean_operador(operador)
    setkey(dt, "operador")
    dt <- dt_op[dt][, op_raw := NULL]
  }

  if ("linea" %in% cols) {
    linea <- cols_unique[grepl("linea", names(cols_unique))]
    col <- clean_column(linea)
    setkey(dt, "linea")
    dt <- col[dt][, col_raw := NULL]
    setnames(dt, old = c("id", "col_clean"), new = c("id.linea", "linea"))
  }

  if ("ruta" %in% cols) {
    ruta <- cols_unique[grepl("ruta", names(cols_unique))]
    col <- clean_ruta(ruta)
    setkey(dt, "ruta")
    dt <- col[dt, on = "ruta"]
  }

  if ("parada" %in% cols) {
    parada <- cols_unique[grepl("parada", names(cols_unique))]
    col <- clean_parada(parada)
    setkey(dt, "parada")
    dt <- col[dt][, c("parada_raw", "id") := NULL]
  }

  #dt <- impute_rutas_by_paradas(dt)

  ## take care of misplaced timestamps
  if (is.null(select.cols) & is.null(dt.col.names)) save_lost_rows(dt)

  ## test for duplicated cenefas after join
  nrow_post <- nrow(dt)
  if (nrow_post != nrow_pre) stop("Duplicated cenefa in clean_parada()")

  save_clean_dt(dt, path)

  cat("Finished file:", basename(path), timetaken(started_at), "\n")

  return(dt)

}
