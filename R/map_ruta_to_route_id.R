#' Map routes from validation files to gtfs routes
#'
#' `map_ruta_to_route_id()` extracts all routes from the validation file and
#' maps them to the routes on the gtfs file based on the bus stops along the
#' routes.
#'
#' @section Files needed to run the function:
#'
#' This function process two different kind of data files, a
#' validation file and a GTFS file. Both files are available for download at:
#'
#' * Validation files at:
#' 'https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal.html'
#'
#' * GTFS files at:
#' 'https://datosabiertos-transmilenio.hub.arcgis.com/search?groupIds=ca6e3d0acf57461d91228659c1b0d2dd'
#'
#' The gtfs files have the geographical data we need, also, this function
#' only requires gtfs files from dates as old as the oldest validation
#' file available. No need to download all of them.
#'
#' Do not modify in any way the names of the downloaded files, the function relies
#' on them.
#'
#' The validation file contains all data from the smart travel cards that users
#' validate when boarding the buses. It has many variables but in this function
#' we only use a few:
#'
#' * 'Rutas' - The name of the bus route
#' * 'Estacion_Parada' - Bus stop code where the user got on the bus
#' * 'Fecha_Transaccion' - A time stamp at boarding time
#' * 'ID_Vehiculo' - Id number of the bus taken
#'
#' @section The problem to solve:
#'
#' With these four variables is possible to recreate the routes for each bus in
#' the system. We are only missing the geographical coordinates of the bus stops
#' to create an accurate bus trip. This is where the GTFS file comes in.
#' The validation file does not provide geo coordinates, only bus stops codes,
#' on the other hand, the GTFS file does have route information and geo coordinates
#' for bus stops. The main problem to map the routes between these two files
#' is that the route names they use are different in each file. This problem
#' can be solve via bus stop code names which are common on both files.
#'
#' `map_ruta_to_route_id()` solves the above mentioned problem by mapping the
#' bus stops from the validation file to the bus stops on the GTFS file allowing
#' to map the routes in both files.
#'
#' @param date A character string in the 'YYYY-MM-DD' format with the date of the
#'   validation file to process, this date is found on the name of the file. Mind
#'   that the date on the file name is in a 'YYYYMMDD' format with no dashes, do
#'   not use that format, it will run into an error.
#' @param val_file_path A character string with the path to the input validation files.
#' @param gtfs_path A character string with the path to the gtfs files.
#'
#' @return `map_ruta_to_route_id()` returns a data.table with clean names for
#'   routes and bus stops with all data needed to create a GTFS feed from the
#'   original validation file.
#'
#' @export
map_ruta_to_route_id <- function(date,
                                 val_file_path = "../data_sets/sitp/validaciones_TM/dia",
                                 gtfs_path = "~/R/data_sets/sitp/gtfs/") {
  raw_dt <- load_val_routes(date = date,
                            val_file_path = val_file_path)

  # a list of data.tables each with a route and its stops to match against the
  # GTFS file
  stops_by_route <- get_unique_stops_by_route(raw_dt)

  # a vector with all GTFS file's paths available that fit the validation file's date
  gtfs_to_unzip <- gtfs_to_unzip(date = date,
                                 gtfs_path =  gtfs_path)
  
  gtfs <- read_gtfs_by_date(gtfs = gtfs_to_unzip,
                           gtfs_files = "routes")

  # find route matches in gtfs files
  gtfs_routes <- match_routes_in_gtfs(rutas = stops_by_route,
                                     gtfs = gtfs)

  # find gtfs trips for `route_id`s
  gtfs_trips <- find_gtfs_trips(routes =  gtfs_routes,
                                gtfs_path = gtfs_path)

  complete_trips <- select_complete_trips(trips = gtfs_trips)

  gtfs_stops <- get_gtfs_stops(trips = complete_trips,
                               date = date,
                               gtfs_path = gtfs_path)
}

get_gtfs_stops <- function(trips, date, gtfs_path) {
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

find_gtfs_trips <- function(routes, gtfs_path) {
  gtfs_files <- routes[, sort(unique(gtfs_file))]
  gtfs_file_name <- list_names_to_file_names(gtfs_files)

  # make a list with `route_id`s by gtfs files
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

  names(gtfs_trips) <- gtfs_files

  return(gtfs_trips)
}

select_complete_trips <- function(trips) {
  names <- names(trips)
  complete_trips <- lapply(
    X = trips,
    FUN = function(dt) {
      # select trips with the largest number of stops by route_id
      trips_max_stops <- dt[, .N, keyby = .(route_id, trip_id)
                            ][, .SD[which.max(N)], keyby = route_id
                              ][, trip_id]

      # filter raw_trips to get trips_max_stops
      complete_trips <- dt[trips_max_stops, on = "trip_id"]
    }
  )
  # keep the file names
  names(complete_trips) <- names

  return(complete_trips)
}

list_names_to_file_names <- function(names) {
  gtfs_file_name <- gsub("(GTFS)\\.([0-9]{4})\\.([0-9]{2})\\.([0-9]{2}).*",
                         "\\1-\\2-\\3-\\4.zip",
                         names)
}

#' Get Unique Stops By Route
#'
#' `get_unique_stops_by_route()` creates a list of data.tables for each route in
#' the validation file.
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

#' Split Prefix for Double Letter Route's name
#'
#' Split the route's name prefix for route names with double letter name, this
#' routes missed a match on the first try because the gtfs routes names don't
#' use double letter prefix.
#' @param rutas A character vector with unique names for routes that have a double
#'   letter prefix on its name.
#' @return A character vector with unique names with single letter prefix
split_prefix <- function(rutas) {
  special <- rutas[grepl("^TC", rutas)]
  double_letter <- rutas[grepl("^[A-Z]{2}", rutas)]
  double_letter <- setdiff(double_letter, special)
  non_dl <- setdiff(rutas, double_letter)
  prefix <- split_dl(double_letter)
  prefix <- prefix[, union(prefix1, prefix2)]
  rutas_to_match <- union(non_dl, prefix)
  rutas_to_match <- union(rutas_to_match, special)

}

split_dl <- function(routes) {
  dt <- data.table::data.table(route_short_name = routes)

  dt[, c("letters", "numbers") := data.table::tstrsplit(route_short_name,
                                                        "(?<=[A-Za-z])(?=\\d)",
                                                        perl = TRUE)]

  dt[,
     `:=`(
       prefix1 = paste0(substr(letters, 1, 1), numbers),
       prefix2 = paste0(substr(letters, 2, 2), numbers))
     ]

  dt[, c("letters", "numbers") := NULL]

  return(dt)

}

#' Match Routes in GTFS
#'
#' `match_routes_in_gtfs()` takes route names from the validation file and tries
#' to find matches on multiple gtfs files sorted by date. First it tries matches
#' on the most recent gtfs file and stores this matches. Failed route matches
#' are then processed further by spliting the double letter prefix from their
#' name and  are matched again against the latest gtfs file and stored . Matches that
#' fail this second round are then tried against the older gtfs files until they
#' get a match in any of them. The trial goes from the most recent gtfs file down
#' to the oldest to avoid matching duplicates, at the end of the loop, if there
#' are routes that did not find a match, they get discarded throwing a warning.
#' @param rutas A list of data.tables, one data.table for each route.
#' @param gtfs A list of gtfs data.tables with route names to match to.
match_routes_in_gtfs <- function(rutas, gtfs) {
  # get a vector with unique route_short_name
  routes_to_find <- unlist(
    lapply(
      X = rutas,
      FUN = \(r) r[, unique(route_short_name)]
    )
  )

  # first find matches in the latest gtfs file
  last <- length(gtfs)
  latest_gtfs <- gtfs[[last]]$routes

  matches <- latest_gtfs[routes_to_find,
                         .(route_id, route_short_name),
                         on = "route_short_name"]

  no_match <- matches[is.na(route_id), unique(route_short_name)]
  matches_rnd_1 <- matches[!is.na(route_id)]

  # Split the double letter prefix and try another match, it's here
  # that we lose the chance to map gtfs routes names to validation routes names
  routes_to_find <- split_prefix(no_match)
  matches <- latest_gtfs[routes_to_find,
                         .(route_id, route_short_name),
                         on = "route_short_name"]

  matches_rnd_2 <- matches[!is.na(route_id)]

  # Up to here we have match all posible routes on the lates gtfs file, now
  # we'll try to find matches in the old gtfs files until we get a match for all
  # routes

  # set variables to be updated in the for loop
  no_match <- matches[is.na(route_id), unique(route_short_name)]
  matches_by_date <- list()

  # check gtfs length, if we only got one gtfs file, skip for loop
  if (last > 1) {
    # find matches from latest to earliest gtfs files, drop the file we just did
    rev_seq <- rev(seq(last - 1))
    for (i in rev_seq) {
      date <- names(gtfs)[i]
      matches <- gtfs[[i]]$routes[no_match,
                                  .(route_id, route_short_name),
                                  on = "route_short_name"]

      # add a col for the gtfs' file date
      matches[, gtfs_file := rep(date, .N)]
      # update the missing routes
      no_match <- matches[is.na(route_id), unique(route_short_name)]
      # update the matche's list
      matches_by_date[[i]] <- matches[!is.na(route_id)]
    }
  }

  matches_latest <- data.table::rbindlist(list(matches_rnd_1,
                                               matches_rnd_2))

  matches_latest[, gtfs_file := rep(names(gtfs)[last], .N)]
  matches_by_date <- data.table::rbindlist(matches_by_date)
  all_matches <- data.table::rbindlist(list(matches_by_date,
                                            matches_latest))

  all_matches <- unique(all_matches)

  lost_routes <- paste(no_match, collapse = " ")
  warning("No match for routes: ", lost_routes)

  return(all_matches)

}

#' Select The GTFS Files By Date
gtfs_to_unzip <- function(date, gtfs_path) {
  gtfs_zip_files <- list.files(
    gtfs_path,
    full.names = TRUE,
    pattern = "GTFS-[0-9]{4}-[0-9]{2}-[0-9]{2}.zip"
  )
  # get date from file name, a vector of all dates available
  gtfs_dates <- sub(
    ".*([0-9]{4}-[0-9]{2}-[0-9]{2}).*",
    "\\1",
    gtfs_zip_files
  )

  # There are gtfs files from 2020 up to 2025, we want dates from late 2022 up to
  # and including the date of the raw file being processed
  select_dates <- gtfs_dates[
    gtfs_dates >= "2022-12-15" & gtfs_dates <= date
  ]

  # paste a regex to filter files
  date_rgx <- paste0(select_dates, collapse = "|")

  # filter files by rgx
  gtfs_to_unzip <- gtfs_zip_files[grepl(date_rgx, gtfs_zip_files)]
}

#' Read GTFS by Date
#'
#' Load the gtfs files to match routes against.
#'
#' @param gtfs list of gtfs file paths
#' @param files Character vector with file names to load, e.g. "routes" or "stop_times"
read_gtfs_by_date <- function(gtfs, gtfs_files) {
  gtfs_by_date <- lapply(
    X = gtfs,
    FUN = \(z) gtfstools::read_gtfs(path = z,
                                    files =  gtfs_files)
  )

  # format gtfs file names to get the dates
  names(gtfs_by_date) <- make.names(basename(gtfs))

  return(gtfs_by_date)

}

#' Extract a Clean Route Name From Validation File's Routes Column
#'
#' @param raw_rutas A character vector with unique route's name from
#'   validation file.
#' @return A data.table with clean route names
get_short_name <- function(raw_rutas) {
  rutas <- data.table::data.table(ruta = raw_rutas)
  rgx_1 <- "^\\([[:alnum:]]+\\) +(.*)" # clean ()s
  rgx_2 <- "^([[:alnum:]]+[-]?[[:alnum:]]+?)[ _].*" # get the code
  rutas[, route_short_name := {
    removed_parenthesis <- sub(rgx_1, "\\1", ruta)
    sub(rgx_2, "\\1", removed_parenthesis) # get the code
  }]

  # special cases
  rutas[
    ,
    route_short_name := sapply(
      X = route_short_name,
      FUN = function(r) {
        if (grepl("^5_2_", r)) "5-2"
        else if (grepl("^7_", r)) "7"
        else if (grepl("^KL$", r)) "KL307"
        else if (grepl("^L6816", r)) "L816"
        else if (grepl("^\\(.*\\)$", r)) "ERROR"
        else r
      }
    )
  ]
  rutas <- rutas[route_short_name != "ERROR"]
}

#' Load Route's Data From Validation File
load_val_routes <- function(date, val_file_path) {
  val_file <- find_files_by_date(from = date)
  val_file <- file.path(val_file_path, val_file)
  dt <- data.table::fread(file = val_file,
                          select = list("factor"= c("Linea",
                                                    "Ruta",
                                                    "Estacion_Parada",
                                                    "ID_Vehiculo"
                                                    ),
                                        "POSIXct" = "Fecha_Transaccion"),
                          col.names = c("linea",
                                        "ruta",
                                        "parada",
                                        "bus_id",
                                        "t_stamp")
                          )
}

#' Get Cenefa
#'
#' Utility function to extract clean stop codes from validation file column
#' parada.
#'
#' @param parada_vector A chracter vector with unique bus stop names
#' @return A character vector with clean stop codes
get_cenefa <- function(parada_vector) {
  cenefas <- sapply(
    X = parada_vector,
    FUN = function(p) {
      rgx <- ".*([0-9]{3}[A-Z][0-9]{2}).*"
      if (grepl(rgx, p)) {
        p <- sub(rgx, "\\1", p)
      } else NA
    }
  )
}

#TODO this is not doing anything!!! delete it
## filter_trips_by_service_id <- function(gtfs_stops, service_id) {
##   gtfs_path <- "../data_sets/sitp/gtfs/"
##   gtfs_files <- lapply(X = gtfs_stops,
##                        FUN = \(dt) dt[, unique(gtfs_file)])

##   gtfs_files <- list_names_to_file_names(gtfs_files)

##   gtfs_calendar_trips <- lapply(X = gtfs_files,
##                                 FUN = function(f) {
##                                   gtfs_file <- file.path(gtfs_path, f)
##                                   gtfstools::read_gtfs(
##                                                path = gtfs_file,
##                                                files = c("calendar", "trips")
##                                              )
##                                 })
## }

match_val_routes_to_route_id <- function(gtfs_stops, stops_by_route) {
  gtfs_routes <- lapply(gtfs_stops, \(dt) dt[, .(route_id = list(unique(route_id)), gfs_file = unique(gtfs_file)), by = route_short_name])
  gtfs_routes <- data.table::rbindlist(gtfs_routes)

  val_routes <- lapply(stops_by_route, \(dt) dt[, .(ruta = unique(ruta), route_short_name = unique(route_short_name))])
  val_routes <- data.table::rbindlist(val_routes)

  matches <- val_routes[gtfs_routes, on = "route_short_name", nomatch = NULL]
  nomatch <- val_routes[gtfs_routes, on = "route_short_name"][is.na(ruta)]

  nomatch[, route_num := sub(".*([0-9]{3})", "\\1", route_short_name)]
  nomatch[, prefix1 := paste0(substr(route_short_name[1], 1, 1), route_short_name[.N]),
          by = route_num]


  matches_rnd_2 <- val_routes[nomatch, on = "route_short_name==prefix1", nomatch = NULL]
  matches_rnd_2[, c("i.ruta", "route_num") := NULL]
  nomatch <- val_routes[nomatch, on = "route_short_name==prefix1"][is.na(ruta)]

  nomatch[, route_short_name := paste0(substr(i.route_short_name[.N], 1, 1), i.route_short_name[1]),
          by = route_num]
  nomatch[, c("ruta", "i.ruta", "route_num") := NULL]

  matches_rnd_3 <- val_routes[nomatch, on = "route_short_name", nomatch = NULL]

  all_matches <- data.table::rbindlist(list(matches, matches_rnd_2, matches_rnd_3), fill = TRUE)
}

clean_raw_dt <- function(raw_dt) {
  clean_routes <- get_short_name(unique(raw_dt$ruta))
  clean_routes[, `:=`(dir_A = sapply(X = route_short_name,
                                     FUN = function(r) {
                                       if (grepl("^[A-DF-HKL]{2}", r)) {
                                         A <- paste0(substr(r, 1, 1), substr(r, 3, 200))
                                       } else NA
                                     }),
                      dir_B = sapply(X = route_short_name,
                                     FUN = function(r) {
                                       if (grepl("^[A-DF-HKL]{2}", r)) {
                                         B <- substr(r, 2, 20)
                                       } else NA
                                     })
                      )
               ]

  paradas <- raw_dt[, .(parada = unique(parada))]
  paradas[, stop_code := get_cenefa(parada)]

  clean_raw_dt <- raw_dt[clean_routes, on = "ruta"
                         ][paradas, on = "parada"
                           ][, c("linea", "ruta", "parada") := NULL]

}
