## Code to prepare `festivos` data set goes here
##
## Create data.table with holydays dates

library("data.table")

festivos <- fread("~/R/data_sets/sitp/otros_datos/festivos/festivos_84-25.csv")

usethis::use_data(festivos, overwrite = TRUE, compress = "xz")
