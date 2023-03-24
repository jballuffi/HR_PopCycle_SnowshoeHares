
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]



# run function to extract  ---------------------------------------------------------------


#function that calculates the overlap of individuals 
HR_overlap <- function(DT, x, y, idcol, utmzone){
  data.xy <- DT[, .(x, y)]
  xysp <- SpatialPoints(data.xy)
  proj4string(xysp) <- CRS(utmzone)
  sppt <- data.frame(xysp)
  idsp <- subset[, .(idcol)]
  coordinates(idsp)<-sppt
  ud <- kernelUD(idsp[,1])
  kernelUD(idsp[,1], h = "href", grid = 200, same4all = TRUE, hlim = c(0.1, 1.5),
           kern = c("bivnorm"), extent = 2)
  overlap <- kerneloverlap(idsp[,1], grid=200, method="HR", percent=90, conditional=TRUE)
  return(overlap)
}


#run function by grid and winter, rename columns
out <- gps[, HR_overlap(DT = .SD, x = x_proj, y = y_proj, idcol = id, utmzone = utm7N), by = .(grid, winter)]
setnames(out, "V1", "overlap")




#remove cases where overlap is 1.00 because this is a home compared against itself (matrix design)
out <- out[!overlap == 1.00]

# #take mean overlaps by grid and winter
# means <- out[, .(mean(overlap), .N), by = .(grid, winter)]
# setnames(means, c("V1", "N"), c("mean_overlap", "pair_N"))

#get total number of individuals sampled per grid and per winter
sample <- gps[, length(unique(id)), by = .(grid, winter)]
setnames(sample, "V1", "gridwinter_N")
sample[, winter_N := sum(gridwinter_N), by = winter]

#merge mean overlaps and individual sample size by grid and winter. This is the final result
overlaps <- merge(out, sample, by = c("grid", "winter"), all.x = TRUE)
overlaps[, overlap := round(overlap, 3)]


(overlapfig <-
  ggplot(overlaps)+
  geom_boxplot(aes(x = winter, y = overlap, color = grid), outlier.shape = NA)+
  geom_text(aes(x = winter, y = .9, label = winter_N))+
  labs(y = "Proportion overlap")+
  theme_minimal())






