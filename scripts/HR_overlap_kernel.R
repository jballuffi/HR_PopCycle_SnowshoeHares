
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]


subset <- gps[winter == "2015-2016"]


HR_overlap <- function(DT){
  data.xy <- DT[, .(x_proj, y_proj)]
  xysp <- SpatialPoints(data.xy)
  proj4string(xysp) <- CRS(utm7N)
  sppt <- data.frame(xysp)
  idsp <- subset[, .(id)]
  coordinates(idsp)<-sppt
  ud <- kernelUD(idsp[,1])
  kernelUD(idsp[,1], h = "href", grid = 200, same4all = TRUE, hlim = c(0.1, 1.5),
           kern = c("bivnorm"), extent = 0.5)
  overlap <- kerneloverlap(idsp[,1], grid=200, method="HR", percent=90, conditional=TRUE)
  return(overlap)
}


out <- subset[, HR_overlap(DT = .SD)]
#this works but it doesnt work when you try to run the function by anything. whats up with that?


