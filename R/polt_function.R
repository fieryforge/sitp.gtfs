plot_route <- function(r1, r2 = NULL) {
  dev.new(width = 12, height = 12)
  x1 <- r1[, stop_lon[-c(1, length(stop_lon))]]
  y1 <- r1[, stop_lat[-c(1, length(stop_lat))]]
  x2 <- r2[, stop_lon[-c(1, length(stop_lon))]]
  y2 <- r2[, stop_lat[-c(1, length(stop_lat))]]
  par(xaxs = "i", yaxs = "i")
  plot(x = x1, y = y1,
       type = "n",
       xlab = "Longitud",
       ylab = "Latitud")
  points(x = x1,
         y = y1,
         type = "o",
         pch = 16, col = "deepskyblue", cex = 1.2)
  points(x = x2,
         y = y2,
         type = "o",
         pch = 18, col = "magenta1", cex = 1.2)
  text(x = x1[1], y = y1[1], adj = c(-0.4, 0))
  text(x = x2[1], y = y2[1], adj = c(-0.4, 0))

}
