#' Map stops from validation files to gtfs routes
#'
#' @description `map_stops_to_route_id()` extracts the routes data from the
#' validation file (read the details section for the extracted variables) to
#' recreate the trips made by the buses on the different routes by day. With
#' this trips table it then selects and extracts the requiered data from the
#' gtfs files to map the routes on the validation file to the routes on the
#' gtfs files, it then returns a data.table with the mapped routes based on
#' the bus stops matched in both files.
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
#' The validation file contains all data from the smart travel cards that users
#' validate when boarding the buses. It has many variables but in this function
#' we only use a few:
#'
#' * 'Rutas' - The name of the bus route
#' * 'Estacion_Parada' - Bus stop code where the user got on the bus
#' * 'Fecha_Transaccion' - A time stamp at boarding time
#' * 'ID_Vehiculo' - Id number of the bus taken
#'
#' The gtfs files have the geographical data we need, also, this function
#' only requires gtfs files from dates as old as the oldest validation
#' file available. No need to download all of them.
#'
#' Do not modify in any way the names of the downloaded files, the function relies
#' on them as they are.
#'
#' @section How to map the routes in both files:
#'
#' The main problem to map the routes between these two files is that
#' the route names are different in each file. This is solved via bus stop code
#' names which are common on both files.
#'
#' The validation file has bus codes but lacks their geographical coordinates,
#' the function takes those from the GTFS files.
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
#'   routes and bus stops with all data needed to recreate a GTFS feed from the
#'   original validation file.
#'
#' @export
map_stops_to_route_id <- function(date, val_file_path, gtfs_path) {
  # load validation file
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


}
