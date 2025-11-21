#' sitp.gtfs: Tools for frequency analysis of the routes in Bogotá's public bus
#' system SITP
#'
#' Useful functions for analyzing SITP route frequencies from the user
#' validation data files published by Transmilenio.
#'
#' @docType package
#' @name sitp.gtfs
#' @aliases sitp.gtfs-package
#'
#' @importFrom data.table := .I .SD %chin% .GRP .N rbindlist uniqueN
#' @importFrom utils globalVariables
#' @importFrom gtfstools read_gtfs
#' @importFrom sf st_as_sf st_geometry st_distance
#' @importFrom osrm osrmRoute
#' @importFrom leaflet leaflet addTiles addMarkers addPolygons
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(
  c(".")
)
