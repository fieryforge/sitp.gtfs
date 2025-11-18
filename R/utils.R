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

#' Select the gtfs files by date
#'
#' `gtfs_to_unzip` returns a character vector with GTFS files selected by dates.
#'
#' @param date A character string in the 'YYYY-MM-DD' format with the date of the
#'   validation file to process.
#' @param gtfs_path A character string with the path to the gtfs files.
#'
#' @returns A character vector with GTFS files selected by dates.
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
#' Load the gtfs files to match routes against them.
#'
#' @param gtfs_zip list of gtfs file paths
#' @param gtfs_files Character vector with file names to load, e.g. "routes" or "stop_times"
#'
#' @return A list of GTFS data.tables
read_gtfs_by_date <- function(gtfs_zip, gtfs_files) {
  gtfs_by_date <- lapply(
    X = gtfs_zip,
    FUN = \(z) gtfstools::read_gtfs(path = z,
                                    files =  gtfs_files)
  )

  # format gtfs file names to get the dates
  names(gtfs_by_date) <- make.names(basename(gtfs_zip))

  return(gtfs_by_date)

}

# format list names to gtfs file names
list_names_to_file_names <- function(names) {
  gtfs_file_name <- gsub("(GTFS)\\.([0-9]{4})\\.([0-9]{2})\\.([0-9]{2}).*",
                         "\\1-\\2-\\3-\\4.zip",
                         names)
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

# do the actual spliting of the prefix
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

plot_route <- function(r1, r2 = NULL) {
  dev.new(width = 12, height = 12)
  x1 <- r1[, stop_lon[-c(1, length(stop_lon))]]
  y1 <- r1[, stop_lat[-c(1, length(stop_lat))]]
  x2 <- r2[, stop_lon[-c(1, length(stop_lon))]]
  y2 <- r2[, stop_lat[-c(1, length(stop_lat))]]
  par(xaxs = "i", yaxs = "i")
  plot(x = x1, y = y1,
       type = "n",
       xlab = "Longitud",
       ylab = "Latitud")
  points(x = x1,
         y = y1,
         type = "o",
         pch = 16, col = "deepskyblue", cex = 1.2)
  points(x = x2,
         y = y2,
         type = "o",
         pch = 18, col = "magenta1", cex = 1.2)
  text(x = x1[1], y = y1[1], adj = c(-0.4, 0))
  text(x = x2[1], y = y2[1], adj = c(-0.4, 0))

}
