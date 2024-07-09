

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- readRDS("output/results/dailyharedensities.rds")

#import food add bunnies
foodadd <- readRDS("data/food_adds.rds")



# make just a density data frame -------------------------------------------

#merge hare density and predation risk
hdensity[, mnth := month(date)]
densities <- merge(hdensity, predrisk, by = c("mnth", "winter"), all.x = TRUE)



# merge densities with home ranges ------------------------------------------------------

#reclassify date
areas[, date := ymd(weekdate)]
#set id as factor
areas[, id := as.factor(id)]

#merge hare density by day of week and winter
DT1 <- merge(areas, densities, by = c("date", "winter"), all.x = TRUE)



# food add -----------------------------------------------------------

DT2[winter == "2018-2019" & date < 2019-01-01, Food := 0] #Sho's food adds didn't start till Jan



# Save final data sets -----------------------------------------------------

#save merged data
saveRDS(DT3, "output/results/compileddata.rds")

#save just densities
saveRDS(densities, "output/results/densities.rds")



