#function that calculates mcp area from a data.table or data.frame
#Arguments: x = x coord; y = y coord; col1-col3 = additional data to include
# utmzone = the utm zone; df = the data.table

mcp_area <- function(gpsdata, x, y, utmzone) {
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(gpsdata[, .SD, .SDcols = c(x, y)],
                                 data = gpsdata,
                                 proj4string = CRS(utmzone))
  
  #calculate mcp size
  area <- mcp.area(spdf, percent = 90, plotit = FALSE)
  
  return(area)
}

