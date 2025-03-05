#' Load rutas zonales from TM argis data set
load_rz_csv <- function() {
  f <- "../sitp/data/aux_data/rutas/Rutas_Zonales_SITP_2024-08.csv"
  d_ruta <- fread(f)
  d_ruta <- d_ruta[, c(2, 16, 14, 5, 11)]
  setnames(d_ruta, new = c("linea", "ruta", "origen", "destino",
                           "longitud.ruta"))

  ## TODO, how to join to DT, siffixes are different, maybe using linea
}
