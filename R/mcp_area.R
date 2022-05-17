

#function that calculates mcp area from a data.table or data.frame
#Arguments: x = x coord; y = y coord; col1-col3 = additional data to include
# utmzone = the utm zone; df = the data.table

mcp_area <- function(x, y, col1, col2, col3, utmzone, dt){
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(dt[, .(x, y)],
                                 data = dt[, .(col1, col2, col3)],
                                 proj4string = CRS(utmzone))
  
  #calculate mcp size
  size <- as.character(mcp.area(spdf, percent = 95, plotit = FALSE))
  
  #return mcp size
  return(size)
}
