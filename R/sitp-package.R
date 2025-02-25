#' @title \strong{sitp: Clean data for the Sistema Integrado de Transporte Publico de Bogotá}
#'
#' @name sitp.analisis
#' @aliases sitp
#' @author Federico Viviescas Ramírez \email{federicoviviescas@hotmail.com}
#' @keywords package
#' @description The sitp-package provides functions to clean and analyse data downloaded from
#' the Transmilenio official data site. The data includes boarding information for users of
#' the public transport network of Bogotá on the daily basis. The data sets record the
#' information from the moment the users validate their bus cards when boarding the bus.
#' The data contains variables such as pickup geographic coordinates with time, bus route taken and
#' seventeen other variables usefull to analyse the bus system and its dynamics.
#' TODO: this must be explained in detail somewhere else, perhaps on the documentation of
#' examples data sets
#'
#' @importFrom utils download.file object.size
#' @importFrom sf st_as_sf
#'
'_PACKAGE'
utils::globalVariables(c(".",
                         "Estacion_Parada",
                         "cenefa",
                         "col_clean",
                         "col_raw",
                         "column",
                         "fecha",
                         "fecha_str",
                         "id",
                         "id.parada",
                         "latitud",
                         "longitud",
                         "nombre",
                         "op_raw",
                         "parada",
                         "parada_raw",
                         "suffix_grp",
                         "timestamp"))
