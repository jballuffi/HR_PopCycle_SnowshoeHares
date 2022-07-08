
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]

#items we will measure home ranges by
weeksplit <- c("id", "winter", "burst", "week", "weekdate")



# calculate weekly MCP areas ----------------------------------------------

#MCP size at 90% and 50%, keep id, winter, season, and grid
#save as RDS

#MCP at 90%
area90 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 90), by = weeksplit]
setnames(area90, "a", "HRninety") #change column name

#MCP at 75%
area75 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 75), by = weeksplit]
setnames(area75, "a", "HR75") #change column name

#MCP at 50%
area50 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 50), by = weeksplit]
setnames(area50, "a", "HRfifty") #change column name




# merge and save output ---------------------------------------------------


#merge areas of 90% , 75% volume together
areas.temp <- merge(area90, area75, by = weeksplit)
#merge the 50% volume as well
areas<- merge(areas.temp, area50, by = weeksplit)


#save HR areas as an RDS file in the output folder
saveRDS(areas, "output/results/hrareas.rds")
