rutas_add_geo <- function(dt, ruta = NULL) {
  r <- ruta
  ruta_geo <- dt[ruta == r, .(cenefa, longitud, latitud), by = ruta
                 ][!duplicated(cenefa)
                   ][!is.na(longitud) & !is.na(latitud)]

  ruta_geo <- st_as_sf(ruta_geo, coords = c("longitud", "latitud"),
                       crs = 3116)
}
