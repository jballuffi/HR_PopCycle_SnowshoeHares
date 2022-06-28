

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- fread("data/Hare_density_monthly.csv")

#import lynx densities
ldensity <- fread("data/Lynx Density_simple.csv")

#import weight results
weights <- readRDS("output/results/bodymass.rds")

#import fod add bunnies
foodadd <- readRDS("data/food_adds.rds")


# clean hare density ------------------------------------------------------

#classify months as either early or late winter for year stuff
late <- c(1, 2, 3, 4)
early <- c(10, 11, 12)

#change col names
setnames(hdensity, "Year", "winter")
setnames(hdensity, "hdensity", "haredensity")

#pull months, years, and days into separate col
hdensity[, mnth := month(Time)]
hdensity[mnth %in% early, y := tstrsplit(winter, "-", keep = 1)]
hdensity[mnth %in% late, y := tstrsplit(winter, "-", keep = 2)]
hdensity[, day := day(Time)]

#create a date col
hdensity[, date := dmy(paste0(day, "-", mnth, "-", y))]

#recalculate hare density from hectare to 100km2
hdensity[, haredensity := haredensity*10000]

#last step: remove time col
hdensity[, Time := NULL]


# merge densities ---------------------------------------------------------

#rename lynx data
names(ldensity) <- c("winter", "ltracks", "ltrack_se", "ltrack_lower", "ltrack_upper", "lynxdensity")

#subset just two columns of interest
lynx <- ldensity[, .(winter, lynxdensity)]

#merge hare and lynx densities together
densities <- merge(hdensity, lynx, by = "winter", all.x = TRUE)

#calculate Pred:Prey ratio
densities[, ppratio := lynxdensity/haredensity]


# merge all data together ----------------------------------------------------------

#set id as factor
areas[, id := as.factor(id)]
foodadd[, id := as.factor(Eartag)]


#merge weights with area
DT1 <- merge(areas, weights, by = c("id", "winter", "season"), all.x = TRUE)

#merge in food adds
DT2 <- merge(DT1, foodadd, by = c("id", "winter"), all.x = TRUE)
DT2[, Eartag := NULL] #remove extra eartage col from food adds
DT2[is.na(Food), Food := 0] #haares with NA in food add get zero to rep control

#merge in densities
DT3 <- merge(DT2, densities, by = c("winter", "season"), all.x = TRUE)

#save merged data
saveRDS(DT3, "output/results/compileddata.rds")

#save just densities
saveRDS(densities, "data/densities.rds")
