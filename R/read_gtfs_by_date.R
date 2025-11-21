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
