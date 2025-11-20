#' Removes the first or last point that creates a loop in gtfs trips
#'
#' Some gtfs trips have errors with duplicated stops, for some reason not yet
#' known, the last stop of the trip is linked to the first stop creating a loop
#' for some trips. `fix_gtfs_trips()` removes the points that creates those
#' loops.
#'
#' @param trips A data.table returned by `get_gtfs_stops()`.
fix_gtfs_trips <- function(trips) {
  data.table::setkeyv(trips, c("route_id", "stop_sequence"))
  sf_trips <- lapply(X = unique(trips$route_id)[719:722],
                     FUN = function(r) {
                       st_r <- sf::st_as_sf(trips[r, on = "route_id"],
                                            coords = c("stop_lon", "stop_lat"),
                                            crs = 4326)
                       st_r_clean <- remove_trouble_point(st_r)
                     })

}

remove_trouble_point <- function(st_r) {
  # Calculate consecutive distances
  distances <- numeric(nrow(st_r) - 1)

  for(i in 1:(nrow(st_r) - 1)) {
    distances[i] <- sf::st_distance(st_r[i, ], st_r[i + 1, ])
  }

  # Calculate median distance
  median_distance <- median(distances)

  # Check conditions for removing first or last point
  first_distance <- distances[1]  # Distance between pt1 and pt2
  last_distance <- distances[length(distances)]  #Distance between ptn and ptn-1

  if (first_distance > (2 * median_distance)) {
    # Remove point 1
    st_r_clean <- st_r[-1, ]
    message("Removed first point: distance = ",
            first_distance, " > 2 * median = ", 2 * median_distance)
  } else if (last_distance > (2 * median_distance)) {
    # Remove point N
    st_r_clean <- st_r[-nrow(st_r), ]
    message("Removed last point: distance = ",
            last_distance, " > 2 * median = ", 2 * median_distance)
  } else {
    # Keep both points
    st_r_clean <- st_r
    message("No points removed - both endpoints within acceptable distance")
  }
  return(st_r_clean)
}
