
#' Match Routes in GTFStf
#'
#' `match_routes_in_gtfs()` matches route names from the validation files to
#' route ids in the GTFS files. It returns a data.table with all route ids
#' that match to a route name in the validation file.
#'
#' @details `match_routes_in_gtfs()` takes route names from the validation file
#' and finds matches on gtfs files sorted by date. First it tries matches
#' on the most recent gtfs file and stores this matches. Failed route matches
#' are then processed further by spliting the double letter prefix from their
#' name and  are matched again against the latest gtfs file and stored . Matches that
#' fail this second round are then tried against the older gtfs files until they
#' get a match in any of them. The trial goes from the most recent gtfs file down
#' to the oldest to avoid matching duplicates, at the end of the loop, if there
#' are routes that did not find a match, they get discarded throwing a warning.
#'
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
