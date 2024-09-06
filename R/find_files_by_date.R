#' Find files by date
#'
#' Find one or more data files (.csv | .rds) by date ranges.
#'
#' @param type Type of file to find: One of `csv` or `rds`.
#' @param from A string in format <Year-Month-Day> like `2024-02-11`. The range
#' starts on this date. If @param until is NULL, only this date is searched.
#' @param to A string in format <Year-Month-Day> like `2024-02-11`. This is
#' the end date of the date range to search for.
#' @param day_num A single digit from 0 to 7 where:
#' 0 = search holidays only, 1 = Sunday, 2 = Monday ... and 7 = Saturday.
#' Dates are filtered by day of the week.
#' @param include_holidays A logical `TRUE` or `FALSE` to filter the search:
#' TRUE includes holidays, FALSE excludes them.
#'
#' @details
#' This function finds data files based on date ranges created from user input.
#' These ranges can be filtered by day of the week and by holiday dates.
#' The function returns a vector with paths for available data files in a given
#' date range.
#'
#' Path to csv files: "data/raw_data/val_dia/"
#' Path to rds files: "data/processed_data/val_dia"
#'
#' @return
#' A vector with paths for files found in a given date range.
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' find_files_by_date(from = "2024-02-11", to = "2024-03-01")
#' }
#'
find_files_by_date <- function(type = NULL, from = NULL, to = NULL,
                               day_num = NULL, include_holidays = FALSE) {
  # Check input parameters
  stopifnot("`type` must be a string: `csv` or `rds` " =
              is.character(type) && grepl("^csv$|^rds$", type),
            "`from` must be a string with format `2024-02-11` Y-Month-Day." =
              is.character(from) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", from),
            "`to` must be a string with format `2024-02-11` Y-Month-Day." =
              is.character(to) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", to)
              || is.null(to),
            "`day_num` must be a single digit from 1 to 7: Sunday = 1 and Saturday = 7." =
              is.numeric(day_num) && inrange(day_num, 0, 7) || is.null(day_num),
            "`include_holidays` must be a logical TRUE or FALSE." =
              is.logical(include_holidays))

##CREATE DATE RANGE FOR THE SEARCH FROM INPUTS
# TODO filter to find holidays only

  # convert parameters to Date class and create a range of dates
  from <- as.IDate(from)
  if (! is.null(to)) { # if NULL search just one date
    to <- as.IDate(to)
    range <- c(from:to)
  } else {
    range <- c(from:from)  #range of one file :D
  }

  # Find files with a date pattern and holidays
  pattern <- ".*([0-9]{4}[0-9]{2}[0-9]{2}).*"
  rds_files <- list.files("data/processed_data/val_dia",
                          full.names = TRUE, pattern = pattern)
  csv_files <- list.files("data/raw_data/reportes_dia",
                          full.names = TRUE, pattern = pattern)
  aux_dir <- "data/aux_data/" # path to file with holiday dates
  dt_holidays <- fread(file = paste0(aux_dir,
                                     "dias_festivos_colombia_1984-2024.csv"))
  hd <- dt_holidays[, fecha_str]

  # Select file type from input
  ifelse(type == "csv", file_type <- csv_files, file_type <- rds_files)

  # Format dates from available files
  pattern <- ".*([0-9]{4})([0-9]{2})([0-9]{2}).*"
  file_dates <- sapply(file_type, \(x) as.IDate(gsub(pattern,
                                                     "\\1-\\2-\\3", x)))

  # Check for file existence in range
  files_begin <- min(file_dates)
  files_end <- max(file_dates)
  b_str <- as.character(as.IDate(files_begin))
  e_str <- as.character(as.IDate(files_end))
  cat("Valid date range:", b_str, "->", e_str, "\n")

  valid_range <- c(files_begin:files_end)
  all_range_is_valid <- all(range %in% valid_range)
  stopifnot("Some dates in range are out of bounds! Try with dates inside the valid range."
             = all_range_is_valid)

  # Filter dates by day of the week
  if (is.numeric(day_num) && day_num > 0) {
    range <- range[wday(range) == day_num]
  } else if (is.numeric(day_num) && day_num == 0) {
    include_holidays <- TRUE # find holidays only
  }

  if (isTRUE(include_holidays) && is.numeric(day_num) && day_num == 0) {
    range <- intersect(range, hd) # find only holidays in range
    # check for holidays in range
    stopifnot("No holidays in range. Try a different valid range."
              = length(range) !=  0)
  }

  # Filter dates to exclude holidays from the search
  if (isFALSE(include_holidays)) {
    range <- setdiff(range, hd) # find only holidays in range
  }
  print(range)

## CREATE DATE RANGE FROM THE FILES AVAILABLE


  # Dates available
  dates_found <- intersect(file_dates, range)

  # complete range with missing dates
  f_start <- as.IDate(min(file_dates))
  f_ends <- as.IDate(max(file_dates))
  complete_range <- c(f_start:f_ends)

  # Print missing dates
  missing_dates <- setdiff(intersect(complete_range, range), file_dates)

  if (length(dates_found) == 0) {
    msg <- "The range given is out of bounds. No available dates for range:"
  } else {
    msg <- "Missing dates for range:"
  }

  print(paste(length(missing_dates), msg, as.IDate(from), "->", as.IDate(to)),
        quote = FALSE)
  print(paste("Missing dates:"), quote = FALSE)
  print(as.IDate(missing_dates))
  # Return path files found
  search_pattern <- gsub("-", "", paste0(as.IDate(dates_found), collapse = "|"))
  files_found <- file_type[grepl(search_pattern, file_type)]
  # msg
  msg <- ifelse(length(dates_found) == 0,
                "No files found for range given.", "Files found for date range")

  print(paste(length(dates_found), msg,
              as.IDate(from), "->", as.IDate(to), ":"), quote = FALSE)

  return(files_found)
}
