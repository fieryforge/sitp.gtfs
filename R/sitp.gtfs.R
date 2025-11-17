#' sitp.gtfs: Herramientas para el análisis de frecuencias de las rutas del
#' sistema de buses públicos de Bogotá SITP
#'
#' Funciones útiles para alaizar las frecuencias de las rutas del SITP a partir
#' de los archivos de datos de validaciones de usuarios publicados por la
#' empresa Transmilenio.
#'
#' @section Usage:
#' Please check the vignettes for more on the package usage:
#' - Basic usage: reading, analysing, manipulating and writing feeds. Run
#' `vignette("gtfstools")` or check it on the [website](
#' https://ipeagit.github.io/gtfstools/articles/gtfstools.html).
#' - Filtering GTFS feeds. Run `vignette("filtering", package = "gtfstools")` or
#' check it on the [website](
#' https://ipeagit.github.io/gtfstools/articles/filtering.html).
#' - Validating GTFS feeds. Run `vignette("validating", package = "gtfstools")`
#' or check it on the
#' [website](https://ipeagit.github.io/gtfstools/articles/validating.html).
#'
#' @docType package
#' @name sitp.gtfs
#' @aliases sitp.gtfs-package
#'
#' @importFrom data.table := .I .SD %chin% .GRP .N rbindlist uniqueN
#' @importFrom utils globalVariables
#' @importFrom gtfstools read_gtfs
#' @importFrom osrm osrmRoute
#' @importFrom leaflet leaflet addTiles addMarkers addPolygons
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(
  c(".")
)
