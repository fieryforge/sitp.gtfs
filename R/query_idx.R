query_idx <- function(rgx) {
  ## query idx for a regex
  lst_raw <- lapply(idx, \(lst) lst[grepl(rgx, lst)])
  ## get dates of raw files that match regex
  raw_dates <- gsub(".*([0-9]{8}).*", "\\1", names(lst_raw[lengths(lst_raw) > 0]))
  ## build file paths
  raw_files <- lapply(raw_dates, \(rd) paste0("../data_sets/test_sitp/validacionZonal",
                                              rd, ".zip"))
  ## build grep command
  dts <- lapply(raw_files, function(rf) {
    fread(cmd = paste("zgrep", rgx, rf))
  })
}
