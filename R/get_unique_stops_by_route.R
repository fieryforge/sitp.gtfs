#' Get unique stops by route
#'
#' `get_unique_stops_by_route()` returns a list of data.tables with each route
#' in the validation file as an element.
#'
#' @section Column names for each data.tables in the list:
#' * ruta: Validation file route name.
#' * parada: Validation file stop name.
#' * route_short_name: Clean stop name to match against the GTFS file, double
#'   letter prefix are not split yet.
#' * stop_code: Clean stop code to match against the GTFS file.
#' @param raw_data A data.table with route's data from the validation file.
#' @return A list of data.tables, a data.table for each route.
get_unique_stops_by_route <- function(raw_dt) {
  stops_by_route <- raw_dt[, .(parada), by = ruta]
  unique_rutas <- stops_by_route[, unique(ruta)]
  stops_by_route <- lapply(
    X = unique_rutas,
    FUN = function(r) {
      dt <- stops_by_route[ruta == r, unique(.SD)]
      dt[,
         `:=`(
           route_short_name = get_short_name(ruta)$route_short_name,
           stop_code = get_cenefa(parada)
         )]
    }
  )
}
