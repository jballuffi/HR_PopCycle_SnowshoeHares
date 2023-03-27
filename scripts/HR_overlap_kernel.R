
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]



# run function to extract  ---------------------------------------------------------------


#function that calculates the overlap of individuals 
HR_overlap <- function(DT, x, y, idcol, utmzone){
  #subset just the x and y coordinates
  data.xy <- DT[, .(x, y)]
  #convert the x y coordinates to spatial points
  xysp <- SpatialPoints(data.xy)
  #set projection
  proj4string(xysp) <- CRS(utmzone)
  #convert back to data frame
  sppt <- data.frame(xysp)
  #assign ID column from original data
  idsp <- DT[, .(idcol)]
  #assign the IDs the coordinates again 
  coordinates(idsp) <- sppt
  #create kernel densities
  ud <- kernelUD(idsp[,1])
  #create home ranges from those kernels 
  kernelUD(idsp[,1], h = "href", grid = 200, same4all = TRUE, hlim = c(0.1, 1.5),
           kern = c("bivnorm"), extent = 2)
  #measure overlap of home ranges
  overlap <- kerneloverlap(idsp[,1], grid=200, method="HR", percent=90, conditional=TRUE)
  return(overlap)
}


#run function by grid and winter, rename columns
out <- gps[, HR_overlap(DT = .SD, x = x_proj, y = y_proj, idcol = id, utmzone = utm7N), by = .(grid, winter)]
setnames(out, "V1", "overlap")




# clean up out put and merge with sample size -----------------------------


#remove cases where overlap is 1.00 because this is a home compared against itself (matrix design)
out <- out[!overlap == 1.00]

#get total number of individuals sampled per grid and per winter
sample <- gps[, length(unique(id)), by = .(grid, winter)]
setnames(sample, "V1", "gridwinter_N")
sample[, winter_N := sum(gridwinter_N), by = winter]

#merge mean overlaps and individual sample size by grid and winter. This is the final result
overlaps <- merge(out, sample, by = c("grid", "winter"), all.x = TRUE)
overlaps[, overlap := round(overlap, 3)]


# figures -----------------------------------------------------------------

(overlapfig <-
  ggplot(overlaps)+
  geom_boxplot(aes(x = winter, y = overlap), outlier.shape = NA)+
  geom_text(aes(x = winter, y = .9, label = winter_N))+
  labs(y = "Proportion overlap")+
  theme_minimal())


# stats -------------------------------------------------------------------

#create table of mean, median, sd
sumoverlap <- overlaps[, .(round(mean(overlap), 3), round(median(overlap), 3), round(sd(overlap), 3), winter_N), by = winter]
names(sumoverlap) <- c("Winter", "Mean", "Median", "Standard deviation", "N")

#anova
mod <- lm(overlaps$overlap ~ overlaps$winter)
amod <- anova(mod) #take ANOVA table from linear regression

#tukey test on ANOVA
aov <- aov(mod)
posthocIR <- TukeyHSD(x = aov, 'overlaps$winter', conf.level = 0.95)
posthocIR





# save things -------------------------------------------------------------

write.csv(sumoverlap, "output/results/overlap_summary.csv")
saveRDS(sumoverlap, "output/results/overlap_summary.rds")

ggsave("output/figures/hr_overlap.jpeg", overlapfig, width = 8, height = 4, units = "in")




