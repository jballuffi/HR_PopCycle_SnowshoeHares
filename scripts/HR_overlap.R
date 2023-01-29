
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]

#items we will measure home ranges by
weeksplit <- c("id", "winter", "burst", "week", "weekdate")


subset <- gps[winter == "2015-2016" & burst == 1 & week == "(-1,7]"]


# spdf <- Spat(utm7N)
#spatialPointsDataFrame(subset[, .SD, .SDcols = c("x_proj", "y_proj")],
#                                data = subset,
#                                proj4string = CRS(utm7N))



data.xy <- subset[, .(x_proj, y_proj)]
xysp <- SpatialPoints(data.xy)
proj4string(xysp) <- CRS(utm7N)
sppt <- data.frame(xysp)
idsp <- subset[, .(id)]
coordinates(idsp)<-sppt
ud <- kernelUD(idsp[,1])
kernelUD(idsp[,1], h = "href", grid = 200, same4all = TRUE, hlim = c(0.1, 1.5),
         kern = c("bivnorm"), extent = 0.5)
image(ud)

overlap <- kerneloverlap(idsp[,1], grid=200, method="HR", percent=95, conditional=TRUE)

