library("data.table")

# where the data files are
rds_files <- list.files("data/processed_data/val_dia", full.names = TRUE)
csv_files <- list.files("data/raw_data/val_dia", full.names = TRUE)
# path for holiday dates
aux_dir <- "data/aux_data/"

#' @name select_files_by_date
#' @description This function selects the available data files by date ranges.
#' @param from A string in format <Year-Month-Day> like `2024-02-11`. The range
#' starts on this date. If @param until is NULL, only this date is selected.
#' @param until A string in format <Year-Month-Day> like `2024-02-11`. This is
#' the end date of the range. It must be a later date then @param from.
#' @param day_num A single digit from 1 to 7 where: Sunday = 1 and Saturday = 7.
#' It filters the date range by day of the week.
#' @param include_holidays A logical `TRUE` or `FALSE` to filter the selection.
#' TRUE = includes holidays in the selection, FALSE excludes them.
select_files_by_dates <- function(from = "", until = NULL, day_num = NULL,
                                  include_holidays = FALSE) {
  # Test input parameters
  stopifnot("`from` must be a string with format `2024-02-11` Y-Month-Day." =
              is.character(from) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", from),
            "`until` must be a string with format `2024-02-11` Y-Month-Day." =
              is.character(until) && grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", until)
              || is.null(until),
            "`day_num` must be a single digit from 1 to 7: Sunday = 1 and Saturday = 7." =
              is.numeric(day_num) && inrange(day_num, 1, 7) || is.null(day_num),
            "`include_holidays` must be a logical TRUE or FALSE." =
              is.logical(include_holidays))

  # Convert parameters to Date class
  from <- as.IDate(from)
  # Test for until parameter, if null get just one date
  if (! is.null(until)) {
    until <- as.IDate(until)
    range <- as.IDate(c(from:until))
  } else {
    range <- from  #Only one file
  }

  # check for available files
  # TODO: Esto está incompleto!!! que hacer con los missing files
  dates_asked <- gsub("-", "", range)
  for (date in dates_asked){
    if (length(which(grepl(date, rds_files))) == 0) {
      print(paste("File not found for date: ", date))
    }
  }

  # Filter files by day of the week
  if (! is.null(day_num)) range <- range[wday(range) == day_num]

  # Filter files to exclude holidays from selection
  if (isFALSE(include_holidays)) {
    # Read file with list of holidays
    dt_holidays <- fread(file = paste0(aux_dir,
                                       "dias_festivos_colombia_1984-2024.csv"))
    hd <- dt_holidays[, fecha_str]
    # pattern to match holiday dates, a long string of dates
    pattern <- paste(hd, collapse = "|")
    # filter dates to exclude holidays
    range <- range[grep(pattern, range, invert = TRUE)]
  }
  # prepare search pattern on all files
  pattern <- gsub("-", "", range)
  pattern <- paste(pattern, collapse = "|")
  # Create list of all files
  selection <- rds_files[grepl(pattern, rds_files)]
  return(selection)
}
