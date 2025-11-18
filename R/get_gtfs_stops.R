#' Get gtfs stops data for trips
#'
#' `get_gtfs_stops()` Joins data.table trips with GTFS stops, route and trips
#' to get all the trip's data in one data.table
#'
#' @param trips A data.table with all the trips found.
#' @param gtfs_path A character vector with the path to the GTFS files.
#'
#' @return A data.table with all the trips found and all data for the stops in
#' those trips.
get_gtfs_stops <- function(trips, gtfs_path) {
  gtfs_file_name <- list_names_to_file_names(names(trips))

  gtfs_files <- file.path(gtfs_path, gtfs_file_name)

  gtfs_stops <- lapply(
    X = gtfs_files,
    FUN = function(f) {
      gtfstools::read_gtfs(
                   path = f,
                   files = c("stops", "routes", "trips"),
                   fields = list(stops = c("stop_id",
                                           "stop_code",
                                           "stop_name",
                                           "stop_lon",
                                           "stop_lat"),
                                 routes = c("route_id",
                                            "route_short_name"),
                                 trips = c("route_id", "service_id")))
    }
  )

  trips_with_stops <- lapply(
    X = seq_along(trips),
    FUN = function(i) {
      dt <- gtfs_stops[[i]][["stops"]][trips[[i]], on = "stop_id"]
      dt <- gtfs_stops[[i]][["routes"]][dt, on = "route_id"]

      # filter trips to find services runing weekdays not sunday

      route_id <- dt[, unique(route_id)]
      trips_by_service_id <- gtfs_stops[[i]][["trips"]][route_id, on = "route_id"]
      no_sunday_routes <- trips_by_service_id[service_id != 1, unique(route_id)]
      dt <- dt[no_sunday_routes, on = "route_id"]
    }
  )
  # keep the names
  names(trips_with_stops) <- make.names(gtfs_file_name)

  trips_by_gtfs_file <- lapply(
    X = seq_along(trips_with_stops),
    FUN = function(i) {
      file_name <- list_names_to_file_names(names(trips_with_stops[i]))
      trips_with_stops[[i]][, gtfs_file := rep(file_name, .N)]
    }
  )

  gtfs_trips <- data.table::rbindlist(trips_by_gtfs_file)

  return(gtfs_trips)
}
