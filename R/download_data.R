#' Download raw data files from the SITP public bus transport system
#'
#' @description This function takes as infput a path to a directory where downloaded files
#' will be saved. The files to download contain data from the validation of
#' users cards once they bord the buses to start a trip.
#'
#' Data includes: Timestamp, route taken, vehicle id, bus stop id, payment info and more.
#'
#' Files come in csv format compressed with the zip program.
#'
#' @param output.data.dir Destination directory for downloaded files
#' 
download_raw_data <- function(output.data.dir = raw.data.dir, url = raw.data.url) {
  if (!dir.exists(output.data.dir))
    stop("Argument 'output.data.dir' does not exist. Pass a valid dir path to store the files.")

  index_html <- base::readLines(url, warn = FALSE)
  find_url <- grep(".*https.*[0-9]{8}(\\.zip|\\.csv).*", index_html, value = TRUE)

  # mind the single double quote in pattern
  files_url <- sub('.*(https.*(\\.zip|\\.csv))".*', "\\1", find_url)
  file_names <- sub(".*(validacionZonal.*(zip|csv)).*", "\\1", files_url)

  # check if files have already been downloaded
  local_files <- basename(list.files(output.data.dir, pattern = "(csv|zip)"))
  files_to_download <- setdiff(file_names, local_files)

  ## If files  available for download, ask user to proceed with the download
  if (length(files_to_download) > 0) {
    cat("Files available for download:\n")
    print(files_to_download)
    message(paste(length(files_to_download), "files will be downloaded."))
    message(paste("Downloaded files will be saved to directory: ", output.data.dir))
    answer <- readline(prompt = "Proceed to download the files? (y/n)")
    if (answer == "y" || answer == "yes") {
      for (f in files_to_download) {
        download.file(url = files_url[grepl(f, files_url)],
                      destfile = paste0(output.data.dir, f),
                      method = "wget")
      }
    }
    else if (answer == "n" || answer == "no")
      stop("No files downloaded.")
    else
      stop("Please answer with a `y` or `n`.")
  }
  else
    stop("No new files available for download.")

  invisible(NULL)
}
