#' Clean column Ruta from raw data files
#'
#' @description
#' Splits the original column Ruta from the raw data file into multiple columns
#'
#' @details
#' From the original character string in column ruta this function creates new
#' columns as follow:
#' - rta.orig: Holds the original character string.
#' - rta.id: Holds just the ruta id in parenthesis.
#' - rta.nombre: Holds just the ruta name without parenthesis.
#' - rta.code: Holds just the ruta's code name.
#' - rta.sufix: The sufix to match the geo data on the rutas shape file.
#'
#' @param rutas A character vector with unique valus for column Ruta from the raw
#' data file
#'
#' @return A data table to be join to the raw data file being process.
cln_raw_ruta <- function(rutas) {
  dt <- data.table(rta_orig = rutas)
  rgx <- "^(\\(.*\\)) (.*)" ## split dt columns into ruta_raw, id, ruta
  dt[, `:=`(rta_id = gsub(rgx, "\\1", rta_orig),
            rta_nombre = gsub(rgx, "\\2", rta_orig))]
  ##clean rta.nombre
  dt[, rta_nombre := gsub("(^ +)(.*)", "\\2", rta_nombre)] #clean begining blank space
  dt[, rta_nombre := gsub(" +", "_", rta_nombre)] #remove all blanks
  dt[, rta_nombre := gsub("_+", "_", rta_nombre)] #only single underscores
  ## get route code name
  dt[, rta_code := gsub("^([A-Z0-9]+|[0-9]+-[0-9]+)_?.*", "\\1", rta_nombre)]
}
