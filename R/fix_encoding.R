#' Clean all paradas for the year 2023
#'
#' This function cleans a particular csv file downloaded from the usual url where
#' the `validaciones diarias` are published. It was uploaded on january 17 2025
#' and found at this url:
#'
#' https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal.html
#'
#' The original file (33 million plus records) contains all validaciones for the
#' year 2024 by parada with geo coordinates and other information.
#'
#' `clean_all_paradas`, does not take any arguments. It returns a data table with
#' all paradas whith geo coordinates.
#'
#' @return A data table

## TODO hay 4 cenefas duplicadas porque tienen en el nombre la misma cenefa
## pero el condigo en parentesis es diferente

clean_paradas <- function() {
  ## load csv from TM arcgis to get clean names
  pz <- fread("data/otros_datos/paradas/2025-01/Paraderos_Zonales_del_SITP.csv",
              select = c("longitud", "latitud", "Y", "X", "nombre", "cenefa" ))

  ## load all paradas 2023
  file <- "data/aux_data/paradas/zonal_2023.zip"
  z_2023 <- fread(file, select = c("Estacion_Parada", "latitud", "longitud"))

  ## get unique coordinates
  dt <- z_2023[, unique(.SD)]

  ## find names with bad_char in Estacion_Parada and replace them
  dt[, Estacion_Parada := clean_bad_character(Estacion_Parada),
  by = .(latitud, longitud)]

  ## remove duplicated rows
  dt <- dt[, unique(.SD)]

  ## create columns id and cenefa
  cenefa_rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*"
  id_rgx <- "^\\(([0-9]{5})\\) ?.*"
  dt[, `:=`(id = gsub(id_rgx, "\\1", Estacion_Parada),
            cenefa = gsub(cenefa_rgx, "\\1", Estacion_Parada))]

  ## fix cenefa with multiple coordinates
  id_con_parent <- "(\\([0-9]+\\)).*"
  dt[duplicated(cenefa), cenefa := gsub(id_con_parent, "\\1", Estacion_Parada)]

  ## replace cenefas with id code for rows with no cenefa on their name
  dt[!grepl(cenefa_rgx, Estacion_Parada), cenefa := gsub(id_con_parent, "\\1", Estacion_Parada)]

  ## join data from transmilenio arcgis to create a data table with all known
  ## cenefas and coordinates.
  dt <- pz[dt, on = "cenefa"]
  dt[, Estacion_Parada := ifelse(is.na(nombre), Estacion_Parada, nombre)]

  ## clean names without cenefa in pz
  m_rgx <- "(^\\(.*\\|)([0-9]+[A-Z][0-9]+|[A-z]+)[_ ](.*)"
  dt[grepl(id_rgx, Estacion_Parada), Estacion_Parada := gsub(m_rgx, "\\3", Estacion_Parada)]

  ## find and replace bad_char, again for the leftovers...
  dt[grepl("�", Estacion_Parada), Estacion_Parada := sapply(Estacion_Parada, replace_error)]

  ## clean paradas without cenefa
  dt[grepl("^\\(", Estacion_Parada), Estacion_Parada := gsub("^\\(.*\\|(.*)", "\\1", Estacion_Parada)]

  setnames(dt, old = c("nombre", "Estacion_Parada", "id", "longitud", "latitud", "i.latitud", "i.longitud"),
           new = c("nombre.pz", "parada", "id_parada", "longitud.pz", "latitud.pz", "latitud", "longitud"))

  setcolorder(dt, c("id_parada", "longitud", "latitud", "cenefa", "parada", "nombre.pz", "longitud.pz", "latitud.pz"))


  fwrite(dt, "data/aux_data/paradas/all_paradas_clean_2023.csv")

  return(dt)

}

#' Auxiliary functions
#' find duplicated names by coordinates, and use the best name for the group
clean_bad_character <- function(names) {
  names <- unlist(names)

  index_dirty <- grepl("�", names)

  if (all(index_dirty)) { ## get the longest dirty name
    return(names[which.max(nchar(names))])
  } else {
    ## get a clean name
    return(names[which.max(nchar(names[!index_dirty]))])
  }
}


#' find the good word to replace the dirty one
replace_error <- function(error) {
  dict_error <- c("Alimentaci�lataforma" = "Alimentación Plataforma",
                  "Alimentaci�n" = "Alimentación",
                  "Alimentaci�" = "Alimentación",
                  "Alquer�a" = "Alquería",
                  "Am�ricas" = "Américas",
                  "Ap�stoles" = "Apóstoles",
                  "Bachu�" = "Bachué",
                  "Bol�var" = "Bólivar",
                  "B�rbara" = "Bárbara",
                  "Conexi�alle" = "Conexión Calle",
                  "Estaci�R" = "Estación KR",
                  "Estaci�adelena" = "Estación Madelena",
                  "Estaci�anderas" = "Estación Banderas",
                  "Estaci�alle" = "Estación Calle",
                  "Estaci�anta" = "Estación Santa",
                  "Estaci�eneral" = "Estación General",
                  "Estaci�erminal" = "Estación Terminal",
                  "Estaci�icentenario" = "Estación Bicentenario",
                  "Estaci�l" = "Estación Cl",
                  "Estaci�n" = "Estación",
                  "Estaci�olinos" = "Estación Molinos",
                  "Estaci�" = "Estación",
                  "Fontib�n" = "Fontibón",
                  "Ingl�s" = "Inglés",
                  "In�s" = "Inés",
                  "Jerusal�n" = "Jerusalén",
                  "Joaqu�del" = "Joaquín El",
                  "Jos�" = "José",
                  "M�xico" = "México",
                  "Nicol�s" = "Nicolás",
                  "Orqu�deas" =  "Orquídeas",
                  "Para�so" = "Paraíso",
                  "Porci�ncula" = "Porciúncula",
                  "Rep�blica" = "República",
                  "Rinc�n" = "Rincón",
                  "R�o" = "Río",
                  "Sim�n" = "Simón",
                  "Tri�ngulo" = "Tríangulo",
                  "T�cnico" = "Técnico",
                  "V�Amapolas" = "Vía Amapolas",
                  "v�a" = "vía")

  ## this function must be called by Reduce function
  replace_bad_char <- function(error, word) {
    gsub(word, dict_error[[word]], error)
  }

  good_word <- Reduce(replace_bad_char, names(dict_error), init = error)

  return(good_word)

}
