#' Find gtfs trips
#'
#' `find_gtfs_trips()` takes as input a data.table with col 'route_id' with the
#' routes that got a matched along with col 'gtfs_file' with the date of the
#' files to look for. It then filters the GTFS stop_times file to load the trips
#' for those routes. Finally, it filters those trips to extract the longest
#' trip for each route and returns a data.table of trips.
#'
#' @param routes A data.table with col 'route_id', the variable to
#' filter the trips.
#' @param gtfs_path A character vector with the path to the GTFS files.
#'
#' @return A data.table with all the trips found
find_gtfs_trips <- function(routes, gtfs_path) {
  gtfs_files <- routes[, sort(unique(gtfs_file))]
  gtfs_file_name <- list_names_to_file_names(gtfs_files)

  # make a list of vectors  with `route_id`s by gtfs files
  route_id_list <- sapply(
    X = routes[, unique(gtfs_file)],
    FUN = \(f) routes[gtfs_file == f, unique(route_id)]
  )

  # if we got just one gtfs file, coerce to list route_id_list, otherwise
  # it comes out as a matrix form the previous sapply
  if (!is.list(route_id_list)) route_id_list <- list(route_id_list)

  # make pattern files for grep by gtfs file
  pattern_files <- sapply(
    X = seq_along(route_id_list),
    FUN = function(i) {
      # append a comma at the end of each pattern to grep with -F option
      route_comma <- paste0(route_id_list[[i]], ",")
      f <- tempfile()
      write(x = route_comma,
            file = f,
            ncolumns = 1)
      f
    }
  )

  # use grep to filter the files before fread, it's faster
  gtfs_trips <- lapply(
    X = seq_along(gtfs_files),
    FUN = function(i) {
      gtfs_file <- file.path(gtfs_path, gtfs_file_name[i])
      command <- paste0("unzip -caq ",
                        gtfs_file,
                        " stop_times.txt |grep -F -f ",
                        pattern_files[i],
                        " |cut -d , -f 1,4,5")

      data.table::fread(cmd = command,
                        col.names = c("trip_id",
                                      "stop_id",
                                      "stop_sequence"))
    }
  )

  # extract `route_id` from col `trip_id`
  gtfs_trips <- lapply(
    X = gtfs_trips,
    FUN = function(dt) {
      dt[, route_id := sub(".*([ZT]_.*)$", "\\1", trip_id)]
      data.table::setkeyv(dt, c("route_id", "trip_id", "stop_sequence"))
      return(dt)
    }
  )

  gtfs_trips <- lapply(
    X = gtfs_trips,
    FUN = function(dt) {
      # select trips with the largest number of stops by route_id
      longest_trip <- dt[, .N, keyby = .(route_id, trip_id)
                            ][, .SD[which.max(N)], keyby = route_id
                              ][, trip_id]

      # filter raw_trips to longest trip
      gtfs_trips <- dt[longest_trip, on = "trip_id"]
    }
  )

  names(gtfs_trips) <- gtfs_files

  return(gtfs_trips)
}
