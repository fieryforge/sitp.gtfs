#' Find validation files by date
#'
#' `find_files_by_date()` returns a character vector of file names that match the
#' input 'date'.
#'
#' @param from A character vector in the format 'YYYY-MM-DD'.
#' @param to A character vector in the format 'YYYY-MM-DD'.
#' @param day_num A single digit from 0 to 7 where: 0 = search holidays only,
#'   1 = Sunday, 2 = Monday ... and 7 = Saturday. Dates are filtered by day of
#'   the week or if day_num = 0, search for holidays inside the range.
#' @param exclude_holidays A logical, 'TRUE' exclude holidays from the search.
#' @param val_file_path A character vector with the path to the directory for
#'   the validation files.
#'
#' @details
#' This function finds data files based on date ranges created from user input
#' or a single file if argument 'to' is NULL.
#'
#' These ranges can be filtered by day of the week and by holidays.
#' The function returns a vector of file names for available validation files
#' found in given date range, or a single file if no range is given.
#'
#' @return A character vector with vallidation file's names.
find_files_by_date <- function(from, to = NULL, day_num = NULL,
                               exclude_holidays = FALSE,
                               val_file_path) {
  stopifnot(
    "`val_file_path` does not exits or is empty."
    = dir.exists(val_file_path),
    "`from` must be a string with format `2024-02-11` Year-Month-Day."
    = is.character(from) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", from),
    "`to` must be a string with format `2024-02-11` Y-Month-Day."
    = is.character(to) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", to) || is.null(to),
    "`day_num` must be a single digit from 0 to 7: Sunday = 1 ... Saturday = 7."
    = is.numeric(day_num) && inrange(day_num, 0, 7) || is.null(day_num),
    "`include_holidays` must be a logical TRUE or FALSE."
    = is.logical(exclude_holidays)
  )
  holydays <- festivos$date
  if (! is.null(to))
    range <- seq(data.table::as.IDate(from), data.table::as.IDate(to))
  else
    range <- data.table::as.IDate(from) # single file
  if (! is.null(day_num)) {
    if (day_num == 0) {
      stopifnot("No holidays in range. Try a different valid range."
                = length(intersect(range, holydays)) > 0)
      exclude_holidays <- FALSE
      range <- intersect(range, holydays)
    }
    else
      range <- range[wday(range) == day_num]
  }
  if (exclude_holidays) # filter holydays
    range <- setdiff(range, holydays)


  # extract date from file names
  available_files <- list.files(val_file_path, pattern = "[0-9]{8}\\.zip")
  date_format <- ".*([0-9]{4})([0-9]{2})([0-9]{2}).*"
  available_dates <- data.table::as.IDate(gsub(date_format, "\\1-\\2-\\3", available_files))

  # message user the total number of available of files
  # replace with dashes for date format
  first <- available_dates[1]
  last <- available_dates[length(available_dates)]
  message(length(available_files),
          " total available files in VALID range from ", first, " to ", last, ".")

  # adjust range to avilable files
  valid_dates <- intersect(available_dates, range)

  if (length(valid_dates) == 0)
    stop("No files available for requested range. Try a different VALID range.")

  # Cut outsiders, clean range from dates before and after available file dates
  if (range[1] < available_dates[1]) {
    message(length(range[range < available_dates[1]]),
            " requested dates are outside VALID range. Dates before ", available_dates[1], " not available.")
    range <- range[range >= available_dates[1]] # cut the head
  }
  if (range[length(range)] > available_dates[length(available_dates)]) {
    off_range <- range[range > available_dates[length(available_dates)]]
    message(length(off_range),
            " requested dates outside VALID range after. Dates after ",
            available_dates[length(available_dates)], " not available.")
    range <- range[range <= available_dates[length(available_dates)]] # cut the tail
  }

  # message missing dates inside VALID range
  missing_dates <- setdiff(range, available_dates)
  if (length(missing_dates) > 0) {
    message(length(missing_dates), " missing dates in requested range ", from, " ", to, ":")
    print(missing_dates)
  }
  else
    print("No missing files in range.")

  dates_found_rgx <- paste(gsub("-", "", valid_dates), collapse =  "|")
  files_found <- list.files(val_file_path, pattern = dates_found_rgx)
  message(length(files_found), " files found for requested date range.")
  return(files_found)
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

plot_raw_points <- function(r1, r2 = NULL) {
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

#' Plot sfc objects
#'
#' @param sfc_object An sf sfc object.
plot_sfc <- function(sfc_object) {
  dev.new(width = 12, height = 12)
  route_id <- sfc_object$route_id[1]
  route_sn <- sfc_object$route_short_name[1]
# Get coordinates
  coords <- sf::st_coordinates(sfc_object)

# Get CRS information
  crs_info <- sf::st_crs(sfc_object)

  plot.default(
    coords[,1], coords[,2],
    xlab = ifelse(crs_info$epsg == 4326, "Longitude", "X"),
    ylab = ifelse(crs_info$epsg == 4326, "Latitude", "Y"),
    main = paste("Ruta -", route_id, "/", route_sn),
    pch = 16,
    col = "purple",
    type = "o",
    asp = 1
  )
  # Add grid for reference
  grid()

  # Optionally add point labels if you have IDs
  text(coords[,1], coords[,2], labels = 1:nrow(coords),
       pos = 3, cex = 0.7, col = "darkgray")

  st_scale_bar_custom(sfc_object, size_km = 1)

}

#' Make scale bar for plots
#'
#' @param sfc_object An sf sfc object.
st_scale_bar_custom <- function(sf_object, pos = "bottomleft", size_km = 1) {
  bbox <- sf::st_bbox(sf_object)

  # Calculate positions based on chosen location
  if (pos == "bottomleft") {
    x_start <- bbox["xmin"] + 0.05 * (bbox["xmax"] - bbox["xmin"])
    y_pos <- bbox["ymin"] + 0.05 * (bbox["ymax"] - bbox["ymin"])
  } else if (pos == "bottomright") {
    x_start <- bbox["xmax"] - 0.15 * (bbox["xmax"] - bbox["xmin"])
    y_pos <- bbox["ymin"] + 0.05 * (bbox["ymax"] - bbox["ymin"])
  }

  # Calculate appropriate length for 1 km based on CRS
  crs <- sf::st_crs(sf_object)
  if (crs$epsg == 4326) {
    # Geographic CRS - approximate conversion
    km_length <- 0.009  # Roughly 1 km at equator
  } else {
    # Projected CRS - use meters directly
    km_length <- 1000 / crs$ud_unit  # This would need adjustment
  }

  # Draw the scale bar
  segments(x_start, y_pos, x_start + km_length, y_pos, lwd = 3)
  text(x_start + km_length/2, y_pos, paste(size_km, "km"), pos = 3)
}


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

get_trip_by_bus <- function(trips, clean_dt) {
  # one way trips
  one_way_routes <- intersect(trips$route_short_name, clean_dt$route_short_name)
  one_way_routes <- clean_dt[one_way_routes, on = "route_short_name"
                             ][is.na(dir_A), unique(route_short_name)]

  double_way_routes <- intersect(trips$route_short_name, clean_dt[!is.na(dir_A), dir_A])
  double_way_routes <- clean_dt[double_way_routes, on = "dir_A"
                                ][!is.na(dir_A), unique(dir_A)]

  # list one way routes
  one_way_trips <- lapply(
    X = one_way_routes,
    FUN = function(r) {
      trip_gtfs <- trips[r, on = "route_short_name"]
      trips_val <- clean_dt[r, on = "route_short_name"
                            ][order(t_stamp)]
      data.table::setkey(trips_val, bus_id) #TODO use autoindex instead

      trip_by_bus <- lapply(
        X = unique(trips_val$bus_id),
        FUN = function(b) {
          trips_val[b, on = "bus_id"
                    ][trip_gtfs[!duplicated(stop_code)],
                      on = c("route_short_name", "stop_code")
                      ][, bus_id := b]
      #TODO number trips by bus
        })
      trips <- rbindlist(trip_by_bus)
    })

  double_way_trips <- lapply(
    X = double_way_routes,
    FUN = function(r) {
      trip_gtfs <- trips[r, on = "route_short_name"]
      trips_val <- clean_dt[r, on = "dir_A"
                            ][order(t_stamp)]
      data.table::setnames(trips_val,
                           old = c("route_short_name", "dir_A"),
                           new = c("mixed_name", "route_short_name"))
      data.table::setkey(trips_val, bus_id) #TODO use autoindex instead

      trip_by_bus <- lapply(
        X = unique(trips_val$bus_id),
        FUN = function(b) {
          trips_val[b, on = "bus_id"
                    ][trip_gtfs[!duplicated(stop_code)],
                      on = c("route_short_name", "stop_code")
                      ][, bus_id := b]
      #TODO number trips by bus
        })
      trips <- rbindlist(trip_by_bus)
    })

  return(list(one_way_trips, double_way_trips))
}

new_get_trip_by_bus <- function(trips, clean_dt) {
  # one way trips
  one_way_routes <- intersect(trips$route_short_name, clean_dt$route_short_name)
  double_way_routes <- intersect(trips$route_short_name, clean_dt[!is.na(dir_A), dir_A])

  # list one way routes
  one_way_trips <- lapply(
    X = one_way_routes,
    FUN = function(r) {
      trip_gtfs <- trips[r, on = "route_short_name"
                         ][!duplicated(stop_code)]

      trips_val <- clean_dt[r, on = "route_short_name"]

      trip_by_bus <- lapply(
        X = unique(trips_val$bus_id),
        FUN = function(b) {
          trips_val[b, on = "bus_id"
                    ][trip_gtfs,
                      on = c("route_short_name", "stop_code")
                      ][, bus_id := b]
      #TODO number trips by bus
        })
      trips <- rbindlist(trip_by_bus)
    })

  double_way_trips <- lapply(
    X = double_way_routes,
    FUN = function(r) {
      trip_gtfs <- trips[r, on = "route_short_name"
                     ][!duplicated(stop_code)]
      trips_val <- clean_dt[r, on = "dir_A"]

      trip_by_bus <- lapply(
        X = unique(trips_val$bus_id),
        FUN = function(b) {
          trips_val[b, on = "bus_id"
                    ][trip_gtfs,
                      on = .(dir_A = route_short_name,
                             stop_code)
                      ][, bus_id := b]
      #TODO number trips by bus
        })
      trips <- rbindlist(trip_by_bus)
    })

  return(list(one_way_trips, double_way_trips))
}
