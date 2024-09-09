#' Download daily validation data of users of the SITP public transport system
#'
#' Return a list of public data files available for download from the
#' Transmilenio's google API. These files contain data from the validation of
#' users cards once they abord the buses to start a trip. Data includes:
#' Timestamp, route taken, vehicle id, bus stop id, payment info and more.
#' Files come in csv format compressed with the zip program.
#'
download_data <- function() {
  download.file(
    url="https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal.html",
    destfile = "data/raw_data/csv_downloads/index.html",
    method = "wget",
    extra = "-v")

  html <- "data/raw_data/csv_downloads/index.html"
  index_html <- base::readLines(html, warn = FALSE)
  find_url <- grep(".*https.*[0-9](\\.zip|\\.csv).*", index_html, value = TRUE)

  # mind the single double quote in pattern
  files_url <- sub('.*(https.*(\\.zip|\\.csv))".*', "\\1", find_url)
  file_names <- sub(".*(validacionZonal.*(zip|csv)).*", "\\1", files_url)

  # check if files have been already downloaded
  local_files <- basename(list.files("data/raw_data/reportes_dia", pattern = "(csv|zip)"))
  files_to_download <- setdiff(file_names, local_files)
  # If files  available for download, ask user to proceed with the download
  if (length(files_to_download) > 0) {
    cat("Files available for download:\n")
    print(files_to_download)
    answer <- readline(prompt = "Proceed to download the files? (y/n)")
    if (answer == "y" || answer == "yes") {
      for (f in files_to_download) {
        download.file(url = files_url[grepl(f, files_url)],
                      destfile = paste0("data/raw_data/reportes_dia/", f),
                      method = "wget")
      }
    } else if (answer == "n" || answer == "no") {
      stop("No files downloaded.")
    } else{
      stop("Please answer with a `y` or `n`.")
    }
  } else {
    stop("No new files available for download.")
  }
}
