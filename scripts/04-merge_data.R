

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- fread("data/Hare_density_simple.csv")

#import lynx densities
ldensity <- fread("data/Lynx Density_simple.csv")

#import trapping data
trapping <- fread("data/Trapping_data_all_records.csv")



# clean trapping data -----------------------------------------------------

trapping[, ID := as.factor(Eartag)]

#grab all unique individuals from home range calculations
inds <- unique(areas$ID)

#subset the trapping data.table to only include these individuals
t2 <- trapping[ID %in% inds]

#turn dateCap column into a date with lubridate function
t2[, date := dmy(dateCap)]

#categorize gps fixes into winters
#should prob turn this into a function
t2[date > "2015-10-31" & date < "2016-04-01", winter := "2015-2016"]
t2[date > "2016-10-31" & date < "2017-04-01", winter := "2016-2017"]
t2[date > "2017-10-31" & date < "2018-04-01", winter := "2017-2018"]
t2[date > "2018-10-31" & date < "2019-04-01", winter := "2018-2019"]

#remove anything that doesn't fall into winter
t2<- t2[!is.na(winter)]

#average mass for early winter season (November and December)


#average mass for late winter season (February and March)
