

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- readRDS("output/results/dailyharedensities.rds")

#import food add bunnies
foodadd <- readRDS("data/food_adds.rds")

#import mortality rates
predrisk <- readRDS("output/results/mortalityrates.rds")

#import trapping data
trapping <- fread("data/Trapping_data_all_records.csv")



# make just a density data frame -------------------------------------------

#merge hare density and predation risk
hdensity[, mnth := month(date)]

densities <- merge(hdensity, predrisk, by = c("mnth", "winter"), all.x = TRUE)



# merge in sex from trapping data ----------------------------------------------

#make eartag a factor
trapping[, id := as.factor(Eartag)]

#turn 0s to NAs
trapping[Sex == 0, Sex := NA]

#grab all unique individuals from home range calculations
inds <- unique(areas$id)

#subset the trapping data.table to only include these individuals
# also subset to remove all cases where weight was not taken
t2 <- trapping[id %in% inds]

#get mode of sex by id to remove any erroenous sex, this function doesnt account for NAs
sexes <- t2[, .(Sex = getmode(Sex)), by = id]

#change to factor
sexes[, Sex := as.factor(Sex)]



# merge densities with home ranges ------------------------------------------------------

#reclassify date
areas[, date := ymd(weekdate)]
#set id as factor
areas[, id := as.factor(id)]

#merge hare density by day of week and winter
DT <- merge(areas, densities, by = c("date", "winter"), all.x = TRUE)



# merge sexes with home range data ----------------------------------------

DT1 <- merge(DT, sexes, by = "id", all.x = TRUE)



# food add -----------------------------------------------------------

DT1[winter == "2018-2019" & date < 2019-01-01, Food := 0] #Sho's food adds didn't start till Jan



# Save final data sets -----------------------------------------------------

#save merged data
saveRDS(DT1, "output/results/compileddata.rds")

#save just densities
saveRDS(densities, "output/results/densities.rds")



