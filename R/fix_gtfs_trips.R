#' Removes the first or last point that creates a loop in gtfs trips
#'
#' Some gtfs trips have errors with duplicated stops, for some reason not yet
#' known, the last stop of the trip is linked to the first stop creating a loop
#' for some trips. `fix_gtfs_trips()` removes the points that creates those
#' loops.
#'
#' @param trips A data.table returned by `get_gtfs_stops()`.
#' @return A list of sfc objects for 'each route_id'.
fix_gtfs_trips <- function(trips) {
  data.table::setkeyv(trips, c("route_id", "stop_sequence"))
  sf_trips <- lapply(X = unique(trips$route_id),
                     FUN = function(r) {
                       sfc_route <- sf::st_as_sf(trips[r, on = "route_id"],
                                            coords = c("stop_lon", "stop_lat"),
                                            crs = 4326)
                       sfc_route_clean <- remove_head_tail(sfc_route)
                     })

}

#' Remove the first or last point in route on condition
#'
#' @param sfc_route An sf sfc route object.
#' @return The input object with points removed if condition met.
remove_head_tail <- function(sfc_route) {
  head_distance <- st_distance(sfc_route[1, ], sfc_route[2, ])
  tail_distance <- st_distance(sfc_route[nrow(sfc_route), ], sfc_route[nrow(sfc_route) - 1, ])

  distance_difference <- abs(head_distance - tail_distance)
  distance_threshold <- units::set_units(500, "m")

  if (distance_difference > distance_threshold) {
    if (head_distance > tail_distance) {
      sfc_route <- sfc_route[-1, ]
      sfc_route$stop_sequence <- seq(nrow(sfc_route))
      message("Removed first point on route: ", sfc_route$route_id[1], " distance diffence = ", distance_difference)
    } else {
      sfc_route <- sfc_route[-nrow(sfc_route), ]
      message("Removed last point on route: ", sfc_route$route_id[1], " distance diffence = ", distance_difference)
    }
  } else {
    message("No points removed on route: ", sfc_route$route_id[1])
    sfc_route
  }

  return(sfc_route)
}
