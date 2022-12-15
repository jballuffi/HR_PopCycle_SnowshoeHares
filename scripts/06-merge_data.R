

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- readRDS("output/results/dailyharedensities.rds")

#import lynx densities
ldensity <- fread("data/Lynx Density_simple.csv")

#import weight results
weights <- readRDS("output/results/bodymass.rds")

#import fod add bunnies
foodadd <- readRDS("data/food_adds.rds")


# make just a density dataframe -------------------------------------------

#rename lynx data
names(ldensity) <- c("winter", "ltracks", "ltrack_se", "ltrack_lower", "ltrack_upper", "lynxdensity")

#subset just two columns of interest
lynx <- ldensity[, .(winter, lynxdensity)]

#cut the hare density data into important cols
hdensity <- hdensity[, .(winter, date, haredensity, winterday, phase)]

#merge by winter
densities <- merge(hdensity, ldensity, by = "winter", all.x = TRUE)

#create pred:prey
densities[, ppratio := lynxdensity/haredensity]

#delete 31 days from the winterday col because HR data starts at November 1st, not October 1st
densities <- densities[winterday >= 32]
densities[, winterday := winterday - 31]


# merge densities with home ranges ------------------------------------------------------

#reclassify date
areas[, date := ymd(weekdate)]
#set id as factor
areas[, id := as.factor(id)]

#merge hare density by day of week and winter
DT1 <- merge(areas, densities, by = c("date", "winter"), all.x = TRUE)


# merge food add -----------------------------------------------------------

foodadd[, id := as.factor(Eartag)]
foodadd[, Eartag := NULL] #remove extra eartage col from food adds

#merge in food adds
DT2 <- merge(DT1, foodadd, by = c("id", "winter"), all.x = TRUE)
DT2[is.na(Food), Food := 0] #hares with NA in food add get zero to rep control
DT2[winter == "2018-2019" & date < 2019-01-01, Food := 0] #Sho's food adds didn't start till Jan

DT2[, Food := as.factor(Food)]

# merge in weights by winter ----------------------------------------------


#merge weights with area
DT3 <- merge(DT2, weights, by = c("id", "winter"), all.x = TRUE)




#save merged data
saveRDS(DT3, "output/results/compileddata.rds")

#save just densities
saveRDS(densities, "output/results/densities.rds")
