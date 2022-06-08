

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- fread("data/Hare_density_simple.csv")

#import lynx densities
ldensity <- fread("data/Lynx Density_simple.csv")

#import weight results
weights <- readRDS("output/results/bodymass.rds")

#import fod add bunnies
foodadd <- readRDS("data/food_adds.rds")


# clean hare density ------------------------------------------------------

#split season off of season year column
hdensity[, season := tstrsplit(Season_year, " ", keep = 1)]
hdensity[, year := tstrsplit(Season_year, " ", keep = 2)]

#reclassify year
hdensity[, year := as.integer(year)]

#rename seasons to match other data
hdensity[season == "Fall", season := "early"]
hdensity[season == "Spring", season := "late"]

#remove earlier years
hdensity <- hdensity[year > 2010]

#create a winter column to match other data
hdensity[season == "early", winter := paste(year, "-", year+1)]
hdensity[season == "late", winter := paste(year-1, "-", year)]

#remove spaces that were created when pasting winter column
hdensity[, winter := gsub(" ", "", winter)]

#remove useless columns
hdensity[, Season_year := NULL]

#rename
setnames(hdensity, "Density", "haredensity")

#recalculate hare density from hectare to 100km2
hdensity[, haredensity := haredensity*10000]

# merge densities ---------------------------------------------------------

#rename lynx data
names(ldensity) <- c("winter", "ltracks", "ltrack_se", "ltrack_lower", "ltrack_upper", "lynxdensity")
#subset just two columns of interest
lynx <- ldensity[, .(winter, ltracks, lynxdensity)]

#merge hare and lynx densities together
densities <- merge(hdensity, ldensity, by = "winter", all.x = TRUE)

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
