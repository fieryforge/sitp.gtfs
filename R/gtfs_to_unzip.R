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
