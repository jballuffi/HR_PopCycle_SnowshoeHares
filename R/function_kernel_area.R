

#current code for MCP

mcp_area <- function(gpsdata, x, y, utmzone, vol) {
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(gpsdata[, .SD, .SDcols = c(x, y)],
                                 data = gpsdata,
                                 proj4string = CRS(utmzone))
  
  #calculate mcp size
  area <- mcp.area(spdf, percent = vol, plotit = FALSE)
  
  return(area)
}

#MCP at 90%
area90 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 90), by = weeksplit]
setnames(area90, "a", "HRninety") #change column name



#kernal_area

kernel_area <- function(gpsdata, utmzone, vol){
  locs <- gpsdata[, .(id, x_proj, y_proj)]
  coordinates(locs) <- c("x_proj", "y_proj")
  proj4string(locs) <- CRS(utmzone)
  kerns <- kernelUD(locs, h = "href", extent = 1) 
  area <- kernel.area(kerns, percent = vol, 
                                  unout = "ha")
  return(area)
}

out <- kernel_area(gpsdata = gps, utmzone = utm7N, vol = 90)





locs <- gps[, .(id, x_proj, y_proj)]
coordinates(locs) <- c("x_proj", "y_proj")
proj4string(locs) <- CRS(utm7N)
kernel.ref <- kernelUD(locs, h = "href", extent = 1) #needs a grid?
vud_points <- getvolumeUD(kernel.ref)
kUD.hr.estimates <- kernel.area(kernel.ref, percent = 90, 
                                unout = "ha")


