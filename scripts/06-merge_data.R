

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


# merge hare densities ------------------------------------------------------

#reclassify date
areas[, date := ymd(weekdate)]
#set id as factor
areas[, id := as.factor(id)]

#cut the hare density data into important cols
hdensity <- hdensity[, .(date, haredensity, winterday)]

#merge hare density by day of week
DT1 <- merge(areas, hdensity, by = "date", all.x = TRUE)


# merge lynx densities ---------------------------------------------------------

#rename lynx data
names(ldensity) <- c("winter", "ltracks", "ltrack_se", "ltrack_lower", "ltrack_upper", "lynxdensity")

#subset just two columns of interest
lynx <- ldensity[, .(winter, lynxdensity)]

#merge in lynx data 
DT2 <- merge(DT1, lynx, by = "winter", all.x = TRUE)


# merge food add -----------------------------------------------------------

foodadd[, id := as.factor(Eartag)]
foodadd[, Eartag := NULL] #remove extra eartage col from food adds

#merge in food adds
DT3 <- merge(DT2, foodadd, by = c("id", "winter"), all.x = TRUE)
DT3[is.na(Food), Food := 0] #hares with NA in food add get zero to rep control
DT3[winter == "2018-2019" & date < 2019-01-01, Food := 0] #Sho's food adds didn't start till Jan



# merge in weights by winter ----------------------------------------------


#merge weights with area
DT4 <- merge(DT3, weights, by = c("id", "winter"), all.x = TRUE)


#save merged data
saveRDS(DT4, "output/results/compileddata.rds")

# #save just densities
# saveRDS(densities, "data/densities.rds")
