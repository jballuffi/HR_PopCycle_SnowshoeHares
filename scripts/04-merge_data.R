

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

#average mass for early winter season (November and December)

#average mass for late winter season (February and March)
