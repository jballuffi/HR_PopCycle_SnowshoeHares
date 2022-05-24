# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#use only sample periods that are 21 days or more
gps <- gps[samplerange >= 21]

#use only sample periods with 21 or more unique days
gps <- gps[uniquedays >= 21]


#MCP size at 90% and 50%, keep ID, winter, season, and grid
#save as RDS

#MCP at 90%
area90 <- gps[, mcp_area(.SD, x = "x.utm", y = "y.utm", utmzone = utm7N, vol = 90), by = .(ID, winter, season)]
setnames(area90, "a", "90") #change column name

#MCP at 90
area50 <- gps[, mcp_area(.SD, x = "x.utm", y = "y.utm", utmzone = utm7N, vol = 50), by = .(ID, winter, season)]
setnames(area50, "a", "50") #change column name

#merge areas of 90% and 50% volume together
areas <- merge(area90, area50, by = c("ID", "winter", "season"))

#save HR areas as an RDS file in the output folder
saveRDS(areas, "output/results/hrareas.rds")


