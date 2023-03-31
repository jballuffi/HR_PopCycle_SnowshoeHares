

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

#import food add bunnies
foodadd <- readRDS("data/food_adds.rds")

#import snow depth
snow <- readRDS("data/snowgrids.rds")



# make just a density data frame -------------------------------------------

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
densities[, winterday := winterday - 31]

densities <- densities[winterday > 0]



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



# merge in snow depth data ------------------------------------------------

#set order
setorder(snow, "snowgrid", "Date")
#change name of date col in snow data
setnames(snow, "Date", "date")

#fill in missing snow depths with the last value (calls backwards in time)
snow[, SD := nafill(SD, "locf"), by = c("winter", "snowgrid")]

#for home range data, when grid is one of the snow grids, just copy to new col snow grid
DT3[grid == "Agnes" | grid == "Kloo" | grid == "Jo", snowgrid := grid]
#all other grids and their closest snow grid, but where is leroy?
DT3[grid == "Sulphur" | grid == "Rolo" | grid == "Chadbear" | grid == "Leroy", snowgrid := "Kloo"]
DT3[grid == "Chitty", snowgrid := "Agnes"]

#function to pull mean snowdepth from the full week of home range data 
weeklysnow <- function(dt, d){
  #take three days before home range date and three days after
  datelist <- c(dt$d -3, dt$d-2, dt$d-1, dt$d, dt$d+1, dt$d+2, dt$d+3)
  #pull the snowgrid from the home range paper
  g <- dt$snowgrid
  #in the date list and snow grid of the "snow" data, average the snow depth
  snowdepths <- snow[date %in% datelist & snowgrid %in% g, mean(SD, na.rm = TRUE)]
  #return only that mean snow depth
  return(snowdepths)
}

#run the function by id and week date
DT3[, SD := weeklysnow(dt = .SD, d = weekdate), by = .(id, weekdate)]



# Save final data sets -----------------------------------------------------


#save merged data
saveRDS(DT3, "output/results/compileddata.rds")

#save just densities
saveRDS(densities, "output/results/densities.rds")



