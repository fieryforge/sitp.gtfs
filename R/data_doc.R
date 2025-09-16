## Data sets used by `clean_raw_data.R`

#' Bus stops in use for the year 2023 by the SITP bus system
#'
#' @description
#' `unique_zonal_2023` is the result of cleaning a much larger raw data file
#' provided by Datos Abiertos TRANSMILENIO S.A. `unique_zonal_2003` is a
#' data table of all bus stops active in 2023 with geo coordinates.
#'
#' @source The original file `zonal_2023.zip` was downloaded from:
#' "https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal/zonal_2023.zip"
#' The file was downloaded the 17th of january 2025.
#'
#' @format A data table with five variables and 7295 rows:
#' \describe{
#'    \item{Estacion_Parada}{The names of bus stops}
#'    \item{longitud}{X coodinate}
#'    \item{latitud}{Y coordinate}
#'    \item{id}{Reference to the original raw data file}
#'    \item{cenefa}{Bus stop id code}
#' }
"unique_zonal_2023"

#' Bus stops with geo coordinates of the SITP zonal system
#'
#' @description
#' This is the official bus stop data set for the year 2024 provided by
#' Datos Abiertos TRANSMILENIO S.A.
#'
#' Visit: "https://datosabiertos-transmilenio.hub.arcgis.com/"
#'
#' @source https://hub.arcgis.com/api/v3/datasets/70b111e96b514bdfb36a7eb532d0eb4f_0/downloads/data?format=csv&spatialRefId=3116&where=1%3D1
#'
#' @format A data table with four variables and 7623 rows:
#' \describe{
#'    \item{Estacion_Parada}{The names of the bus stops}
#'    \item{longitud}{X coodinate}
#'    \item{latitud}{Y coordinate}
#'    \item{cenefa}{The id code of the bus stops}
#' }
"paradas_sitp_geo"

#' An almost complete set of bus stops of the SITP public bus system
#'
#' @description A date set with most of the bus stops that have geographic
#' coordinates found in `unique_zonal_2023` and `paradas_sitp_geo` datasets.
#'
#' @source data comes from `unique_zonal_2023` and `paradas_sitp_geo` data sets.
#'
#' @format A data table with five variables and 7932 rows:
#' \describe{
#'    \item{cenefa}{The id code of the bus stops}
#'    \item{id}{Reference to the original raw data file}
#'    \item{parada}{The names of bus stops}
#'    \item{longitud}{X coodinate}
#'    \item{latitud}{Y coordinate}
#' }
"index_paradas"


#' An almost complete set of bus routes of the SITP public bus system
#'
#' @description A data set with most of the route's names in raw form mapped to
#' a clean name found in the official SITP route's data set.
#'
#' `index_rutas` is used by `clean_raw_data.R` in order to map route's raw names
#' to clean route names.
#'
#' @source "https://hub.arcgis.com/api/v3/datasets/7fd3d61c4a90448f8a88b29f252be2f1_0/downloads/data?format=shp&spatialRefId=3116&where=1%3D1"
#'
#' @format A data table with four variables and 2115 rows:
#' \describe{
#'    \item{ruta}{A clean ruta name}
#'    \item{sufix.lst}{Ruta names with sufix}
#'    \item{ruta_orig}{Ruta names as found in a raw data file}
#'    \item{id}{Ruta id code}
#' }
"index_rutas"

#' Routes of the SITP published by Tullaveplus
#'
#' @description A data set with the route's names in long format with the
#' list of bus stops by name scraped from the Tullaveplus official website.
#'
#' @source "https://www.tullaveplus.gov.co/planea-tu-viaje/frecuencias-y-horarios"
#'
#' @format A data table with six variables and 374 rows:
#' \describe{
#'    \item{id.linea}{Route id code}
#'    \item{tipo}{Route type: one of urbano, complementario o especial}
#'    \item{linea}{line name}
#'    \item{nombre}{Route names in long format as found in a raw data file}
#'    \item{destino}{Route origin and destination}
#'    \item{paradas}{Bus stops address}
#' }
"rts_tullave"

#' A dictionary used to clean the Unicode Replacement Character
#'
#' @description A dictionary to replace bad encoded words.
#'
#' The raw data files this package seeks to clean have suffered
#' of mismatched encoding somewhere along their processing. Transmilenio, which
#' is the public authority charged of making this data accessible to the public
#' seems to have no concerns about this. This dictionary helps to remedy this
#' problem.
#'
#' @source Custom made for this package
#'
#' @format A named character vector with 51 words
"dict_error"

#' SITP bus fleet
#'
#' @description Data set for the SITP bus fleet with no reference to
#' the `TRONCAL` component.
#'
#' @source https://storage.googleapis.com/validaciones_tmsa/FlotaVinculada/flota_vinculada_20250223.csv
#'
#' @format data table with 6 variables and 8278 rows
#' \describe{
#'    \item{id}{Reference to the original raw data file.}
#'    \item{matricula}{License plate.}
#'    \item{codigo_bus}{The id code painted on the bus.}
#'    \item{capacidad}{Maximum number of passangers per bus}
#'    \item{operador}{Name of the company operating the bus}
#'    \item{componente}{A type of service, one of: URBANO, ALIMENTADOR, COMPLEMENTARIO or ESPECIAL}
#' }
"bus_fleet"

#' Lineas from all raw data files
#'
#' @description A data table with lineas from all raw files checked by `idx_all_raw.R`
#' @source `data-raw/idx_all_raw.R` file
#' @format A data table with one variable and 3021 rows
#' \describe{
#'      \item{linea}{lineas from all raw data files available}
#' }
"idx_linea"

#' Rutas from all raw data files
#'
#' @description A data table with rutas from all raw files checked by `idx_all_raw.R`
#' @source `data-raw/idx_all_raw.R` file
#' @format A data table with one variable and 3639 rows
#' \describe{
#'      \item{ruta}{rutas from all raw data files available}
#' }
"idx_ruta"

#' Paradas from all raw data files
#'
#' @description A data table with paradas from all raw files checked by `idx_all_raw.R`
#' @source `data-raw/idx_all_raw.R` file
#' @format A data table with one variable and 11225 rows
#' \describe{
#'      \item{parada}{paradas from all raw data files available}
#' }
"idx_parada"

#' Operadores from all raw data files
#'
#' @description A data table with operadores from all raw files checked by `idx_all_raw.R`
#' @source `data-raw/idx_all_raw.R` file
#' @format A data table with one variable and 24 rows
#' \describe{
#'      \item{operador}{operadores from all raw data files available}
#' }
"idx_operador"
