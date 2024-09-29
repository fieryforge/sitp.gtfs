#' Assing coordenates to paradas
#'
#' @export
#'
#' @import sf
#'
dt <- readRDS("data/processed_data/val_dia/validacionZonal20240801.rds") # load data table
dt_paradas <- dt[, .(cenefa = unique(Estacion_Parada))]
sin_cenefa <- dt_paradas[grepl("^Portal|Estac", cenefa), cenefa]
sin_cenefa <- unique(sub("(\\w+\\ +\\w+).*", "\\1", sin_cenefa))

sf_paradas <- st_read("data/aux_data/paradas_shp/Paraderos_Zonales_del_SITP.shp")
portales_sf <- unique(sf_paradas$nombre[grepl("^Portal", sf_paradas$nombre)])
estaciones_sf <- unique(sf_paradas$nombre[grepl("^Estaci", sf_paradas$nombre)])
paradas_geo <- as.data.table(sf_paradas[, "cenefa"])
