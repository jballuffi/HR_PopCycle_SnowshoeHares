

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- readRDS("output/results/dailyharedensities.rds")

#import mortality rates
predrisk <- readRDS("output/results/mortalityrates.rds")

#import weight results
weights <- readRDS("output/results/bodymass_allweights.rds")
winterweights <- readRDS("output/results/bodymass_bywinter.rds")

#import food add bunnies
foodadd <- readRDS("data/food_adds.rds")

#import snow depth
snow <- readRDS("data/snowgrids.rds")



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



# merge food add -----------------------------------------------------------

foodadd[, id := as.factor(Eartag)]
foodadd[, Eartag := NULL] #remove extra eartage col from food adds

#merge in food adds
DT2 <- merge(DT1, foodadd, by = c("id", "winter"), all.x = TRUE)
DT2[is.na(Food), Food := 0] #hares with NA in food add get zero to rep control
DT2[winter == "2018-2019" & date < 2019-01-01, Food := 0] #Sho's food adds didn't start till Jan
DT2[, Food := as.factor(Food)]



# merge in weights by winter ----------------------------------------------

#subset weight data
weightsub <- weights[, .(id, winter, date, Sex, Weight)]

#merge weight with data keeping all weight data
DT3 <- merge(DT2, weightsub, by = c("id", "winter"), allow.cartesian = TRUE, all = TRUE)

#cut out individuals with weight data but no home range data
DT3 <- DT3[!is.na(M90)]

#subtract weight date from home range date 
DT3[, datediff := abs(as.integer(date.x - date.y))]

#take minimum difference in date 
DT3[, mindiffdate := min(datediff), by = .(id, winter, weekdate)]

DT3 <- DT3[datediff == mindiffdate]

#change name
setnames(DT3, c("date.y", "date.x"), c("dateweight", "date"))

#remove some duplicates from this merge. Not sure how they happened
DT3[, dup := duplicated(date), id]
DT3 <- DT3[dup == FALSE]



# merge in snow depth data ------------------------------------------------

#for home range data, when grid is one of the snow grids, just copy to new col snow grid
DT3[grid == "Agnes" | grid == "Kloo" | grid == "Jo", snowgrid := grid]
#all other grids and their closest snow grid, but where is leroy?
DT3[grid == "Sulphur" | grid == "Rolo" | grid == "Chadbear" | grid == "Leroy", snowgrid := "Kloo"]
DT3[grid == "Chitty", snowgrid := "Agnes"]

#set order
setorder(snow, "snowgrid", "Date")
#change name of date col in snow data
setnames(snow, "Date", "snowdate")
#setnames(snow, "snowgrid", "grid")


#function to pull mean snowdepth from the full week of home range data 
weeklysnow <- function(weekdate, grid) {
  
  #take three days before home range date and three days after
  datelist <- c(
    weekdate - 3,
    weekdate - 2,
    weekdate - 1,
    weekdate,
    weekdate + 1,
    weekdate + 2,
    weekdate + 3
  )
  
  #in the date list and snow grid of the "snow" data, average the snow depth
  snow[snowdate %in% datelist & snowgrid %in% grid, mean(SD)]
}


DT3[, SD := weeklysnow(weekdate = .BY[[1]], grid = snowgrid), by = .(date, id)]




# Save final data sets -----------------------------------------------------

#save merged data
saveRDS(DT3, "output/results/compileddata.rds")

#save just densities
saveRDS(densities, "output/results/densities.rds")



