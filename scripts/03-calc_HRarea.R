# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")


gps <- gps[samplerange >= 21]


gps <- gps[uniquedays >= 21]


#MCP size at 90% and 50%, keep ID, winter, season, and grid
#save as RDS