#' Find files by date
#'
#' Find one or more data files (.csv | .rds) by date ranges.
#'
#' @param type Type of file to find: One of `csv` or `rds`.
#' @param from A string in format <Year-Month-Day> like `2024-02-11`. The
#' search range starts on this date. If @param until is NULL, only this
#' date is searched.
#' @param to A string in format <Year-Month-Day> like `2024-02-11`. This is
#' the last date of the search range.
#' @param day_num A single digit from 0 to 7 where: 0 = search holidays only,
#' 1 = Sunday, 2 = Monday ... and 7 = Saturday. Dates are filtered by day of
#' the week or if day_num = 0, search for holidays inside the range.
#' @param include_holidays A logical `TRUE` or `FALSE` to filter the search:
#' TRUE includes holidays, FALSE excludes them.
#'
#' @details
#' This function finds data files based on date ranges created from user input.
#' These ranges can be filtered by day of the week and by holidays.
#' The function returns a vector with paths for available data files found in
#' given date range.
#'
#' Path to csv files: "data/raw_data/val_dia/"
#' Path to rds files: "data/processed_data/val_dia"
#'
#' @return
#' A vector with paths for data files found in any  given date range.
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' find_files_by_date(from = "2024-02-11", to = "2024-03-01", day_num = 3)
#' }
#'
find_files_by_date <- function(type = NULL, from = NULL, to = NULL,
                               day_num = NULL, include_holidays = FALSE) {
  # Check input parameters
  stopifnot(
    "`type` must be a string: `raw` or `clean` "
    = is.character(type) && grepl("^raw$|^clean$", type),
    "`from` must be a string with format `2024-02-11` Year-Month-Day."
    = is.character(from) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", from),
    "`to` must be a string with format `2024-02-11` Y-Month-Day."
    = is.character(to) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", to) || is.null(to),
    "`day_num` must be a single digit from 0 to 7: Sunday = 1 ... Saturday = 7."
    = is.numeric(day_num) && inrange(day_num, 0, 7) || is.null(day_num),
    "`include_holidays` must be a logical TRUE or FALSE."
    = is.logical(include_holidays))

### CREATE DATE RANGES: FROM INPUT PARAMS AND FROM DATA FILES AVALIABLE

  # convert input parameters to Date class and create a range of dates
  from <- as.IDate(from)
  if (! is.null(to)) {
    to <- as.IDate(to)
    range <- c(from:to)
  } else {
    range <- c(from:from)  #range of one file, looks odd but works :D
  }

  # Find files with a date pattern on it's name
  pattern <- ".*([0-9]{4}[0-9]{2}[0-9]{2}).*"
  rds_files <- list.files("../data_sets/sitp/clean",
                          full.names = TRUE, pattern = pattern)
  csv_files <- list.files("../data_sets/sitp/raw",
                          full.names = TRUE, pattern = pattern)
  # read holidays data.table
  aux_dir <- "../sitp/data/aux_data/" # path to file with holiday dates
  dt_holidays <- fread(file = paste0(aux_dir,
                                     "dias_festivos_colombia_1984-2024.csv"))
  hd <- dt_holidays[, fecha_str]

  # Select file type to search from input paramater
  ifelse(type == "raw", file_type <- csv_files, file_type <- rds_files)

  # Create a vector of dates from available files, format to class IDate
  pattern <- ".*([0-9]{4})([0-9]{2})([0-9]{2}).*"
  file_dates <- sapply(file_type, \(x) as.IDate(gsub(pattern,
                                                     "\\1-\\2-\\3", x)))

  # Create a continous range where all files available fit
  files_begin <- min(file_dates)
  files_end <- max(file_dates)
  valid_range <- c(files_begin:files_end)

  # let user know valid dates to search for
  num_files_available <- length(file_dates)
  begin_rng <- as.character(as.IDate(files_begin))
  end_rng <- as.character(as.IDate(files_end))
  cat("\n", num_files_available, "available files found in valid range:",
      "[", begin_rng, ":", end_rng, "]", "\n")

  # Check if user's date range fits in valid range
  is_valid_range <- all(range %in% valid_range)
  stopifnot("Some dates in range are out of bounds! Use only dates inside the valid range."
            = is_valid_range)

### FILTER USER'S RANGE WITH IMPUTS PARAMETERS

  # Filter dates by day of the week
  if (!is.null(day_num) && day_num > 0) {
    range <- range[wday(range) == day_num]
  } else if (!is.null(day_num) && day_num == 0) {
    # force search for holidays only
    include_holidays <- TRUE
    range <- intersect(range, hd)
    # check for holidays in range
    stopifnot("No holidays in range. Try a different valid range."
              = length(range) >  0)
  }

  # Filter dates to exclude holidays from the search
  if (isFALSE(include_holidays)) {
    range <- setdiff(range, hd)
  }
  # Hopefully we got a valid, clean and filterd range to search for

### FIND DATES IN USER RANGE MATCHING AGAINST AVAILABLE FILES

  dates_found <- intersect(file_dates, range)
  # create a continuos complete range including missing files
  dates_start <- as.IDate(min(file_dates))
  dates_end <- as.IDate(max(file_dates))
  complete_range <- c(dates_start:dates_end)

  # Find missing dates for the search range
  missing_dates <- setdiff(intersect(complete_range, range), file_dates)
  num_missing_dates <- length(missing_dates)
  rng_begin <- as.character(as.IDate(from))
  rng_end <- as.character(as.IDate(to))

  # Tell user about missing files
  msg <- "Missing dates for range:"
  cat(num_missing_dates, msg, "[", rng_begin, ":", rng_end, "]", "\n")
  if (num_missing_dates > 0) {
    cat("Missing dates:\n")
    print(as.IDate(missing_dates))
  }

  # Tell user about files found
  num_files_found <- length(dates_found)
  cat(num_files_found, "Files found for range", "[",
      rng_begin, "->", rng_end, "]","\n")

  # Return vector of file's paths found
  search_pattern <- gsub("-", "", paste0(as.IDate(dates_found), collapse = "|"))
  files_found <- file_type[grepl(search_pattern, file_type)]
  return(files_found)
}
