#' Scrap routes' data from Transmilenio website
#'
#' Download routes' codes, names and ids  and return a data.table with those
#' variables
#' @return A data.table
scrap_routes <- function() {
  url <- "https://ms-transmiapp-rm2xahnybq-uk.a.run.app/api/v1/rutas/buscar"
  h <- curl::new_handle()

  curl::handle_setheaders(
          h,
          "Content-Type" = "application/json"
        )

  curl::handle_setopt(
          h,
          postfields = '{"tipo":null,"troncalId":null,"estacionId":null,"zona":null,"q":null}'
        )

  # get how many routes are available
  res <- curl::curl_fetch_memory(url = url, handle = h)
  res_json <- rawToChar(res$content)
  total_elements <- jqr::jq(res_json, '.totalElements')

  # build url
  url <- paste0("https://ms-transmiapp-rm2xahnybq-uk.a.run.app/api/v1/rutas/buscar?page=0&size=",
                total_elements,
                "&sort=idCodigo,nombre")
  # scrap all routes
  res <- curl::curl_fetch_memory(url = url, handle = h)
  res_json <- rawToChar(res$content)
  all_routes_json <- jqr::jq(res_json,
                             '.content | .[] | [{"codigo": .codigo, "id": .id, "nombre": .nombre, "tipo": .tipo}] | add')

  # json to data.table: Create a temporary connection
  con <- textConnection(all_routes_json)
  # Stream parse the JSON, keep json arrays as lists with simplifyVector = FALSE
  dt_list <- jsonlite::stream_in(con, simplifyVector = FALSE)
  close(con)

  dt <- rbindlist(dt_list)

}
