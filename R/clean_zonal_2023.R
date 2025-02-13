#' Clean all paradas for the year 2023
#'
#' This function cleans the csv file downloaded from the usual url where
#' the `validaciones diarias` are published. It was uploaded on january 17 2025
#' and found at this url:
#'
#' https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal.html
#'
#' The original file (33 million plus records) contains all validaciones for the
#' year 2023 by parada with geo coordinates and other information.
#'
#' `clean_zonal_2023`, does not take any arguments. It returns a data table
#' with all paradas whith geo coordinates for the year 2023.
#'
#' @return A data table
clean_zonal_2023 <- function() {
  ## load csv from TM arcgis to get clean names
  pz_f <- "../sitp/data/otros_datos/paradas/2025-01/Paraderos_Zonales_del_SITP.csv"
  pz <- fread(pz_f, select = c("longitud", "latitud",
                               "Y", "X", "nombre", "cenefa" ))

  ## load all paradas 2023
  ap_f <- "../sitp/data/aux_data/paradas/zonal_2023.zip"
  z_2023 <- fread(ap_f, select = c("Estacion_Parada", "latitud", "longitud"))

  ## get unique coordinates
  dt <- z_2023[, unique(.SD)]

  ## strip col Estacion_Parada, get id
  id_rgx <- "^(\\([0-9]{5}\\)) ?.*"
  dt[, id := gsub(id_rgx, "\\1", Estacion_Parada)]


  ## find duplicated ids and unify names with the longest string
  dt[, Estacion_Parada := Estacion_Parada[which.max(nchar(Estacion_Parada))], by = id]

  ## get cenefas
  cenefa_rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*"
  dt[, cenefa := gsub(cenefa_rgx, "\\1", Estacion_Parada)]

  ## remove duplicated row
  dt <- dt[, unique(.SD)]

  ## add cenefa suffix for duplicated cenefas with different coordinates
  dt[, cenefa := if (.N > 1) paste(cenefa, letters[seq_len(.N)], sep = "_") else cenefa, by = cenefa]

  ## deal with rows with no cenefa, use part of the name as cenefa
  nn_rgx <- "^(\\([0-9]{5}\\) )(.*)\\|.*"
  dt[!grepl(cenefa_rgx, cenefa), cenefa := gsub(nn_rgx, "\\2", cenefa)]

  ## get clean names from pz
  pz_cn <- pz[, .(cenefa, nombre)]
  setkey(pz_cn, cenefa)
  setkey(dt, cenefa)

  dt <- pz_cn[dt, on = "cenefa"]

  ## get clean names
  dt[!is.na(nombre), Estacion_Parada := nombre]

  ## fix names for cenefas not found on pz (eg. rows with col nombre == NA
  m_rgx <- "[^\\|].*\\|([^\\|].*)" ##m_rgx <- "^\\(.*\\|(.*)"
  dt[is.na(nombre), Estacion_Parada := (gsub(m_rgx, "\\1", Estacion_Parada))]

  ## clean Estacion_Parada from leftovers
  p_rgx <- "[0-9]{3}[A-Z][0-9]{2}[_ \\s]"
  dt[is.na(nombre), Estacion_Parada := (gsub(p_rgx, "", Estacion_Parada))]

  ## clean \uFFFD char
  dt[grepl("\uFFFD", Estacion_Parada), Estacion_Parada := sapply(Estacion_Parada, replace_error)]



  ## add cenefas in pz not found in dt
  pz_cenefas <- pz[, unique(cenefa)]
  dt_cenefas <- dt[, unique(cenefa)]
  cenefas <- setdiff(pz_cenefas, dt_cenefas) ## cenefas in pz and not in dt
  pz <- pz[cenefa %in% cenefas, -c("Y", "X")] ## rm unused cols

  dt <- rbindlist(list(dt, pz), fill = TRUE)

  ## get all names to new parada col
  dt[, parada := ifelse(is.na(Estacion_Parada), nombre, Estacion_Parada)]
  dt[, `:=`(nombre = NULL, Estacion_Parada = NULL)]

  ## celan unacceptable chars
  c <- rawToChar(as.raw(c(0xc2, 0xa0)))
  dt[, parada := (gsub(c, "", parada))]
  dt[, parada := (gsub('[_\\|\\"]', "", parada))]

  ## set columns order
  setcolorder(dt, neworder = c("id", "cenefa", "parada", "longitud", "latitud"))
  setnames(dt, old = "id", new = "id.parada")

  f <- "../sitp/data/aux_data/paradas/index_paradas.csv"
  fwrite(dt, f, na = NA)

  return(dt)

}


#' Replace words with unrecognized utf-8 character with valid one
#' @param error Values from column Estacion_Parada with \uFFFD character
replace_error <- function(error) {
  dict_error <- readRDS("../sitp/data/aux_data/paradas/dict_error.rds")

  ## this function must be called by Reduce function
  replace_bad_char <- function(error, word) {
    gsub(word, dict_error[[word]], error)
  }

  good_word <- Reduce(replace_bad_char, names(dict_error), init = error)

  return(good_word)

}
