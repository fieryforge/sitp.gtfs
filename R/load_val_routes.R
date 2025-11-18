#' Load Route's Data From Validation File
#'
#' `load_val_routes()` reads a validation file selected by `date`. It takes as
#' input a date that matches the validation file name and a path to the
#' directory where the validation files are stored. It then reads the selected
#' file and returns a data.table with the variables relevant to process the
#' routes
#'
#' @param date A character string with a date in the format 'YYYY-MM-DD' to
#'   match a date written on the validation file name.
#' @param val_file_path A character string with the path to the directory where
#'   the validation files are stored.
#'
#' @return A data.table with colums needed to process all routes in
#'   the validation file.
load_val_routes <- function(date, val_file_path) {
  val_file <- find_files_by_date(from = date)
  val_file <- file.path(val_file_path, val_file)
  dt <- data.table::fread(file = val_file,
                          select = list("factor"= c("Linea",
                                                    "Ruta",
                                                    "Estacion_Parada",
                                                    "ID_Vehiculo"
                                                    ),
                                        "POSIXct" = "Fecha_Transaccion"),
                          col.names = c("linea",
                                        "ruta",
                                        "parada",
                                        "bus_id",
                                        "t_stamp")
                          )
}
