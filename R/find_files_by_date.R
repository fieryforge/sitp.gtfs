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
                               val_file_path = "~/R/data_sets/sitp/validaciones_TM/dia/") {
  stopifnot(
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
