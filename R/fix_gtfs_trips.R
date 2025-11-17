#' Removes the first or last point that creates a loop in gtfs trips
#'
#' Some gtfs trips have errors with duplicated stops, for some reason not yet
#' known, the last stop of the trip is link to the first stop creating a loop
#' for some trips. `fix_gtfs_trips()` removes the points that creates those
#' loops.
#'
#' @param trips A data.table returned by `get_gtfs_stops()`.
fix_gtfs_trips <- function(trips) {

}
