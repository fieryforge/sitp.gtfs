## Data sets used by `clean_raw_data.R`

#' Bus stops in use for the year 2023 by the SITP zonal system
#'
#' @description
#' `unique_zonal_2023` is the result of cleaning a much larger data file
#' provided by Datos Abiertos TRANSMILENIO S.A. `unique_zonal_2003` is a
#' data table of all bus stops active in 2023 with geo coordinates
#'
#' @source The original file `zonal_2023.zip` was downloaded from:
#' "https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal/zonal_2023.zip"
#' The file was downloaded the 17th of january 2025.
#'
#' @format A data table with five variables and 7295 rows:
#' \describe{
#'    \item{Estacion_Parada}{The names of the bus stops}
#'    \item{longitud}{X coodinate}
#'    \item{latitud}{Y coordinate}
#'    \item{id}{id reference to the original file}
#'    \item{cenefa}{The id code of the bus stops}
#' }
"unique_zonal_2023"

#' Bus stops with geo coordinates of the SITP zonal system
#'
#' @description
#' This is the official bus stop data set for the year 2024 provided by
#' Datos Abiertos TRANSMILENIO S.A.
#'
#' @source The original file `Paraderos_Zonales_del_SITP.csv` was downloaded from:
#' https://datosabiertos-transmilenio.hub.arcgis.com/
#' following the link "TransMiZonal(Componente Zonal)"
#'
#' @format A data table with four variables and 7623 rows:
#' \describe{
#'    \item{Estacion_Parada}{The names of the bus stops}
#'    \item{longitud}{X coodinate}
#'    \item{latitud}{Y coordinate}
#'    \item{cenefa}{The id code of the bus stops}
#' }
"paradas_sitp_geo"
