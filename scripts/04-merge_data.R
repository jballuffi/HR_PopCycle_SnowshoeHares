

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("data/hrareas.rds")

#import hare densities
hdensity <- readRDS("data/dailyharedensities.rds")

#import food add bunnies
foodadd <- readRDS("data/food_adds.rds")

#import mortality rates
predrisk <- readRDS("data/mortalityrates.rds")

#import trapping data
trapping <- fread("data/Trapping_data_all_records.csv")



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



# make just a density + predation risk data frame -------------------------------------------

#merge hare density and predation risk
hdensity[, mnth := month(date)]

densities <- merge(hdensity, predrisk, by = c("mnth", "winter"), all.x = TRUE)



# merge densities with home ranges ------------------------------------------------------

#cut just hare density and date
formerge <- hdensity[, .(date, phase, haredensity)]

#reclassify date
formerge[, date := ymd(date)]

#reclassify date
areas[, date := ymd(weekdate)]

#set id as factor
areas[, id := as.factor(id)]

#merge hare density by day of week and winter
DT <- merge(areas, formerge, by = "date", all.x = TRUE)

#double check how many points in nov
DT[mnth == 11, .N]

#double check that .N of nov is equal to .N of those missing hare density
DT[is.na(haredensity), .N]

#remove any data from november
DT <- DT[!mnth == 11]



# merge sexes with home range data ----------------------------------------

DT1 <- merge(DT, sexes, by = "id", all.x = TRUE)



# food add -----------------------------------------------------------

DT1[winter == "2018-2019" & date < 2019-01-01, Food := 0] #18/19 food adds didn't start till Jan



# Save final data sets -----------------------------------------------------

#save merged data
saveRDS(DT1, "data/compileddata.rds")

#save just densities
saveRDS(densities, "data/densities.rds")



