
#function that calculates the area of a home range using kernel density
#Arguments: the gps data, utmzone = the utm zone, vol = volume density ()


kernel_area <- function(gpsdata, utmzone, vol){
  locs <- gpsdata[, .(x_proj, y_proj)]
  coordinates(locs) <- c("x_proj", "y_proj")
  proj4string(locs) <- CRS(utmzone)
  kerns <- kernelUD(locs, h = "href", extent = 2) 
  area <- kernel.area(kerns, percent = vol, unout = "ha")
  area <- t(area)
  return(area)
}


