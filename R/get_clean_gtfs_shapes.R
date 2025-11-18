#' Get get_clean_gtfs_shapes
#'
#' @description Using the gtfs `stops.txt` file as input, this function returns
#' a new gtfs `shapes.txt` fixing the errors of the original `shapes.txt` file.
#'
#' @return A gtfs `shapes.txt` file.
get_clean_gtfs_shapes <- function(route_id, trips) {
  route <- trips[route_id, on = "route_id"]
  gtfs_file <- route[, gtfs_date]
  stops <- route[, .(cenefas = unlist(cenefas_gtfs))]
  stops[, stop_sequence := seq(length(cenefas))]

  stops_and_shape <- stops_to_shape(gtfs_file, stops$cenefas)

  return(stops_and_shape)
}

stops_to_shape <- function(gtfs_file,
                           cenefas,
                           gtfs_path = "../data_sets/sitp/gtfs/") {

  path <- file.path(gtfs_path, gtfs_file)
  gtfs_stops <- gtfstools::read_gtfs(path = path,
                                     files = "stops")
  # for convenience to access the data.table

  gtfs_stops <- gtfs_stops[["stops"]]

  # get the route stops from gtfs

  trip_stops <- gtfs_stops[cenefas, -c("location_type", "parent_station", "zone_id"), on = "stop_code"]

  # data.table to sf object, col geometry

  trip_stops <- sf::st_as_sf(trip_stops,
                             coords = c("stop_lon", "stop_lat"),
                             crs = 4686)
  # get the actual shape of the route

  trip_shape <- osrm::osrmRoute(loc = trip_stops, overview = "full")

  return(list(trip_stops, trip_shape))
}

map_shape <- function(shape) {
  m <- leaflet(shape[[2]])
  m <- addTiles(m)
  m <- addPolylines(m)
  p <- leaflet(shape[[1]])
  p <- addMarkers(p)
  m <- p
}
