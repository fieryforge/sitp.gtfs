#' Route finder
#'
#' @description
#' Find the route paths based on bus stop geo coordinates and time stamps
find_route_path <- function(raw_file) {
  dt <- load_raw_data(raw_file,
                      select.cols =  c("Ruta", "ID_Vehiculo", "Estacion_Parada",
                                       "Fecha_Transaccion"),
                      dt.col.names =  c("ruta", "bus", "parada", "time_stamp"))
  dt[, bus := as.character((bus))]
  # filter valid time stamps
  date <- gsub(".*([0-9]{8}).*", "\\1", basename(f[888]))
  date <- gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", date)
  d <- as.POSIXct(date, tz = "UTC", time = "00:00:00")
  t1 <- d + (60 * 60 * 4)
  t2 <- t1 + (60 * 60 * 24 -1)
  # 24 hour range from 4AM
  # TODO What to do with validations outside time range
  dt <- dt[time_stamp > t1 & time_stamp < t2, .SD]

  unique_values <- dt[, unlist(lapply(.SD, unique)),
                      .SDcols = c("ruta", "parada")]

  ruta <- unique_values[names(unique_values) %like% "ruta"]
  parada <- unique_values[names(unique_values) %like% "parada"]

  # TODO: fix names to comply with grass naming convention: names must start with a-z, no dots in col names
  r <- cln_raw_ruta(ruta)
  p <- clean_parada(parada)

  p[, `:=`(id = NULL, id.parada = NULL)]

  dt <- r[dt, on = "rta_orig==ruta"]
  dt <- p[dt, on = "parada_raw==parada"]

  # remove rows without coordinates
  dt <- dt[!is.na(longitud), .SD]

  dt  <- dt[, .(rta_code, bus, cenefa, time_stamp, longitud, latitud, .GRP),
            by = rta_nombre]

  sin_N_bus <- dt[, .(longitud, latitud, n_bus_cenefa = uniqueN(bus),
                      t_1 = min(time_stamp), t_n = max(time_stamp), .N),
                  keyby = .(rta_nombre, cenefa)
                  ][, unique(.SD)]

  N_bus <- dt[, .(n_bus_ruta = uniqueN(bus)), keyby = rta_nombre]

  dt <- N_bus[sin_N_bus, on = "rta_nombre"]
  dt[, p_overlap := .N, by = cenefa] ## add overlaping points col

  ## dt to sf
  dt_g <- sf::st_as_sf(dt, coords = c("longitud", "latitud"), crs = 4686)

  ## write geopkg
  pkg_name <- basename(raw_file)
  pkg_name <- gsub(".*([0-9]{8}).*", "\\1", pkg_name)
  pkg_name <- paste0("rutas-", pkg_name)
  dsn <- paste0("../scratch/", pkg_name, ".gpkg")
  geopkg <- sf::st_write(dt_g, dsn =  dsn, append = FALSE)


  ## nombres <- dt[, unique(rta.nombre)]

  ## # Make a list of dts for each route
  ## dts_by_rta <- lapply(dt[, unique(rta.nombre)], \(r) dt[rta.nombre == r])
  ## names(dts_by_rta) <- nombres
  ## ## dts to st
  ## dts_geo <- lapply(dts_by_rta, function(dt) {
  ##   st_as_sf(dt, coords = c("longitud", "latitud"), crs = 4686)
  ## })
  ## ## write geo package
  ## lapply(seq_along(dts_geo), function(i) {
  ##   name <- paste0(names(dts_geo)[i])
  ##   gpkg <- paste0("../scratch/rutas-", date, ".gpkg")
  ##   sf::st_write(dts_geo[[i]], dsn = gpkg, layer =  name)
  ## })

  ## return(dts_geo)

}
