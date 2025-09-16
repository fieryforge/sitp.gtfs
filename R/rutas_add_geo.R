route_to_points <- function(dt) {
  ruta_coord <- dt[, unique(.SD), by = ruta, .SDcols = c("longitud", "latitud")
                   ][!is.na(longitud)]

  route_list <- split(ruta_coord, by = "ruta")

  routes_sf <- lapply(route_list, function(dt) {
    sf::st_as_sf(dt, coords = c("longitud", "latitud"), crs = 4326)
  })

  routes_trans <- lapply(routes_sf, \(x) sf::st_transform(x, crs = 3116))

  points <- lapply(routes_trans, sf::st_geometry)

  unique_route <- dt[, .(ruta = unique(ruta))]

  unique_route[, ruta.pts := lapply(ruta, \(x) find_route_by_points(points[[x]]))]

  #ruta_guessed <-
}

find_route_by_points <- function(points) {
  print(names(points))
  ## mesure against all routes, elapsed 14 seconds
  distances <- lapply(seq_along(rz_names$route_name), ## rz_names should be loaded
                      \(i) sf::st_distance(points, rz_names[i, ]))

  suma <- lapply(distances, sum)

  route_index <- which(suma == (min(unlist(suma))))

  r <- rz_names$route_name[route_index]

}

rutas_add_geo <- function(dt) {
  ruta_geo <- dt[ruta == r, .(cenefa, longitud, latitud), by = ruta
                 ][!duplicated(cenefa)
                   ][!is.na(longitud) & !is.na(latitud)]

  ## geometry units in degrees
  ruta_geo <- st_as_sf(ruta_geo, coords = c("longitud", "latitud"),
                       crs = 4326)

  ## transform to EPSG:3116 (MAGNA-SIRGAS / Colombia Bogota zone)
  ruta_geo_transformed <- sf::st_transform(ruta_geo, crs = 3116)

  ## get just the points
  points <- sf::st_geometry(ruta_geo_transformed)

}



plot_point_linestring <- function(p.object, l.object) {
  plot(sf::st_geometry(l.object), col = "blue", lwd = 2, main = "RUTA and Paradas")
  plot(sf::st_geometry(p.object), col = "red", pch = 16, add = TRUE)
}

test_point_in_line <- function(p.object, l.object) {
  matches <- sf::st_intersects(p.object, l.object, sparse = FALSE)
  print(matches)
  matching_points <- p.object[matches]
  print(matching_points)

  plot(sf::st_geometry(l.object), col = "blue", lwd = 2, main = "Matching points in line")
  plot(sf::st_geometry(p.object), col = ifelse(matches, "red", "grey"), pch = 16, add = TRUE)
  legend("topright", legend = c("On Line", "Not on Line"), col = c("red", "grey"), pch = 16)
}


## Test points are within a threshold distance from the line
test_distance <- function(p.object, l.object) {
  # Calculate distances
  distances <- sf::st_distance(p.object, l.object)

  ## Define the distance threshold (e.g., 1000 meters)
  threshold <- 25.0 # in meters

  ## Test if p.object are within the threshold distance
  within_threshold <- as.numeric(distances) <= threshold

  ## Extract p.object within the threshold distance
  points_within_threshold <- p.object[within_threshold, "geometry"]

  ## Add distances and threshold condition to p.object
  points_with_distances <- sf::st_sf(
                                 cenefa = p.object[, "cenefa"],
                                 geometry = sf::st_geometry(p.object),
                                 distance_to_line = as.numeric(distances),
                                 within_threshold = within_threshold
                               )

  ## Print the p.object with distances and threshold condition
  print(points_with_distances)



}

## Plot the L.OBJECT and p.object
plot_generic <- function() {
  ggplot() +
    geom_sf(data = l.object, color = "blue", size = 1) + # L.OBJECT
    geom_sf(data = points_with_distances, aes(color = within_threshold), size = 3) + # P.Object
    scale_color_manual(values = c("FALSE" = "red", "TRUE" = "green")) + # Color by threshold
    ggtitle("Paradas within 25 meters of the line") +
    theme_minimal()

}
