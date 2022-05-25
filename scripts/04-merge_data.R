

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

#grab all unique individuals
inds <- unique(areas$ID)

#subset the trapping data.table to only include these inds
t2 <- trapping[Eartag %in% inds]

str(t2)
t2[, date := dmy(dateCap)]

#categorize gps fixes into winters
t2[date > "2015-10-31" & date < "2016-04-01", winter := "2015-2016"]
t2[date > "2016-10-31" & date < "2017-04-01", winter := "2016-2017"]
t2[date > "2017-10-31" & date < "2018-04-01", winter := "2017-2018"]
t2[date > "2018-10-31" & date < "2019-04-01", winter := "2018-2019"]


#average mass for early winter season (November and December)


#average mass for late winter season (February and March)
