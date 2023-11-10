#function that calculates mcp from a data.table or data.frame
#Arguments: x = x coord; y = y coord; col1-col3 = additional data to include
# utmzone = the utm zone; gpsdata = the data.table

mcp_calc <- function(gpsdata, x, y, utmzone, vol) {
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(gpsdata[, .SD, .SDcols = c(x, y)],
                                 data = gpsdata,
                                 proj4string = CRS(utmzone))

  M90 <- mcp(spdf, percent = vol)
  
  return(M90)
}

