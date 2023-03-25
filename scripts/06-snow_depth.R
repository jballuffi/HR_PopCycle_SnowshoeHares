
#script to collect snow data that was measured by the team every day on agnes, jo, and kloo


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)




# collect data ------------------------------------------------------------

#list snow files and read in
snowfiles <- dir("data/", "Snow_grid*", full.names = TRUE) 
ls.snowfiles <- lapply(snowfiles, fread)

#rbindlist with an origin column
snows <- rbindlist(ls.snowfiles, fill = TRUE, use.names = TRUE, idcol = "origin")
#now re-assign the origin column the file names
snows[, origin := factor(origin, labels = basename(snowfiles))]



# clean up data -----------------------------------------------------------


#redo the grid names
snows[grep("agnes", origin), snowgrid := "Agnes"]
snows[grep("jo", origin), snowgrid := "Jo"]
snows[grep("kloo", origin), snowgrid := "Kloo"]

#fix up other data cols
snows[, COMMENTS := NULL]
snows[, Date := lubridate::dmy(DATE)]
setnames(snows, "OPEN SD", "SD")

#cut to just three columns of interest
snow <- snows[, .(Date, snowgrid, SD)]

#order snow data by grid first, then date
setorder(snow, snowgrid, Date)



#categorize fixes into winters
snow[month(Date) > 10, winter := paste0(year(Date), "-", year(Date) + 1)]
snow[month(Date) < 4, winter := paste0(year(Date) - 1, "-", year(Date))]

snow <- snow[!is.na(winter)]



#save data
saveRDS(snow, "data/snowgrids.rds")
