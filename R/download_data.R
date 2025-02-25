#' Download raw data files from the SITP public bus transport system
#'
#' This function takes as infput a path to a directory where downloaded files
#' will be saved. These files contain data from the validation of
#' users cards once they bord the buses to start a trip. Data includes:
#' Timestamp, route taken, vehicle id, bus stop id, payment info and more.
#' Files come in csv format compressed with the zip program.
#'
#' @param download_dir Destinatio directory for downloaded files
#' 
download_data <- function(download_dir = "data-raw/downloads/") {
  if (download_dir == "data-raw/downloads/" & !dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  } else if (download_dir != "data-raw/downloads/" & !dir.exists(download_dir)) {
    dir.create(download_dir)
  }

  url="https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal.html"
  index_html <- base::readLines(url, warn = FALSE)
  find_url <- grep(".*https.*[0-9]{8}(\\.zip|\\.csv).*", index_html, value = TRUE)

  # mind the single double quote in pattern
  files_url <- sub('.*(https.*(\\.zip|\\.csv))".*', "\\1", find_url)
  file_names <- sub(".*(validacionZonal.*(zip|csv)).*", "\\1", files_url)

  # check if files have already been downloaded
  local_files <- basename(list.files(download_dir,
                                     pattern = "(csv|zip)"))
  files_to_download <- setdiff(file_names, local_files)

  ## If files  available for download, ask user to proceed with the download
  if (length(files_to_download) > 0) {
    cat("Files available for download:\n")
    print(files_to_download)
    instruction <- paste('Enter "all" without the quotes to download all files',
                         'available or a digit for a single file by index, or',
                         'a range like "7:10" without the quotes to download',
                         'multiple files: ')
    which_files <- readline(prompt = instruction)
    if (which_files != "all") {
      i <- as.integer(unlist(strsplit(which_files, split = ":")))
      if (length(i) > 1) {
        files_to_download <- files_to_download[c(i[1]:i[2])]
        message("Files to download: ")
        print(files_to_download)
      } else {
        files_to_download <- files_to_download[i]
        message("Files to download: ")
        print(files_to_download)
      }
    } else {
      message(paste(length(files_to_download), " files will be downloaded."))
    }
    message(paste("Downloaded files will be saved to directory: ", download_dir))
    answer <- readline(prompt = "Proceed to download the files? (y/n)")
    if (answer == "y" || answer == "yes") {
      for (f in files_to_download) {
        download.file(url = files_url[grepl(f, files_url)],
                      destfile = paste0(download_dir, f),
                      method = "wget")
      }
    } else if (answer == "n" || answer == "no") {
      stop("No files downloaded.")
    } else {
      stop("Please answer with a `y` or `n`.")
    }
  } else {
    stop("No new files available for download.")
  }

  invisible(NULL)
}
