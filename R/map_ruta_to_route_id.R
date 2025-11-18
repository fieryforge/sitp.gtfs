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

  # a vector with all GTFS file paths available for the validation file's date
  gtfs_to_unzip <- gtfs_to_unzip(date = date,
                                 gtfs_path =  gtfs_path)
  
  gtfs <- read_gtfs_by_date(gtfs_zip = gtfs_to_unzip,
                           gtfs_files = "routes")

  # find route matches in gtfs files
  gtfs_routes <- match_routes_in_gtfs(rutas = stops_by_route,
                                     gtfs = gtfs)

  # find the longest gtfs trips for routes matched in gtfs
  gtfs_trips <- find_gtfs_trips(routes =  gtfs_routes,
                                gtfs_path = gtfs_path)

  # get geo coordinates for the stops
  gtfs_stops <- get_gtfs_stops(trips = gtfs_trips,
                               gtfs_path = gtfs_path)
  #TODO clean trips with clear head and tail
  # match stops id to cenefas and separate mix trips
}


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
