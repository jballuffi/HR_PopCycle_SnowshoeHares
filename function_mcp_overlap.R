
mcp_overlap <- function(gpsdata, x, y, utmzone, vol) {
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(gpsdata[, .SD, .SDcols = c(x, y)],
                                 data = gpsdata,
                                 proj4string = CRS(utmzone))
  
  #calculate mcp size
  overlap <- kerneloverlap(spdf[,1], method = "HR", percent = 90)
  
  return(overlap)
}

test <- gps[winter == "2015-2016"]


testout <- test[, mcp_overlap(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 90)]








#this code works on the test data frame to create one home range
spdf <- SpatialPointsDataFrame(test[, .SD, .SDcols = c("x_proj", "y_proj")],
                       data = test,
                       proj4string = CRS(utm7N))
mcp <- mcp(spdf, percent = 90, unin = "m", unout = "ha")




mcp_fun <- function(gpsdata) {
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(gpsdata[, .SD, .SDcols = c("x_proj", "y_proj")],
                                 data = gpsdata,
                                 proj4string = CRS(utm7N))
  mcp <- mcp(spdf, percent = 90, unin = "m", unout = "ha")
  
  return(mcp)
}
mcp <- test[, mcp_fun(.SD)]
plot(mcp)


#create list within test by id
testlist <- split(test, test$id)

#this will make an MCP of 90, creating a spatialpolygonsdataframe
#run on each individual separately using lapply
testout <- lapply(testlist, function(x) {
  #first convert to a spatial points data frame
  spdf <- SpatialPointsDataFrame(x[, .SD, .SDcols = c("x_proj", "y_proj")],
                                 data = x,
                                 proj4string = CRS(utm7N))
  mcp <- mcp(spdf, percent = 90, unin = "m", unout = "ha")
  
  return(mcp)
})

plot(testout[[2]])



#from the internet


## Use some simulated data
ani1 <- SpatialPoints(matrix(rnorm(200, mean=2), ncol=2))
ani2 <- SpatialPoints(matrix(rnorm(200, mean=1), ncol=2))

## Function to calculate overlaps
gOverlap <- function(hr1, hr2, ...) {
  a <- gIntersection(hr1, hr2, ...)
  if (is.null(a)) {
    return(0)
  }
  gArea(a, byid=TRUE) / gArea(hr1, byid=TRUE) 
}

## Calcualte homeranges
hr1 <- mcp(ani1)
hr2 <- mcp(ani2)

## Calculate HR overlap
gOverlap(hr1, hr2)