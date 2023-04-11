
#script to collect snow data that was measured by the team every day on agnes, jo, and kloo


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)




# collect data ------------------------------------------------------------

#list snow files from grid-level measurements and fread
snowfiles <- dir("data/", "Snow_grid*", full.names = TRUE) 
ls.snowfiles <- lapply(snowfiles, fread)
#rbindlist with an origin column
snows <- rbindlist(ls.snowfiles, fill = TRUE, use.names = TRUE, idcol = "origin")
#now re-assign the origin column the file names
snows[, origin := factor(origin, labels = basename(snowfiles))]


#data from camera traps
snowcams <- fread("data/Snow_cameras.csv")


# clean up grid data -----------------------------------------------------------

#redo the grid names
snows[grep("agnes", origin), snowgrid := "Agnes"]
snows[grep("jo", origin), snowgrid := "Jo"]
snows[grep("kloo", origin), snowgrid := "Kloo"]

#fix up other data cols
snows[, COMMENTS := NULL]
snows[, Date := lubridate::dmy(DATE)]
setnames(snows, "OPEN SD", "SD")

#cut to just three columns of interest
gsnow <- snows[, .(Date, snowgrid, SD)]

#order snow data by grid first, then date
setorder(gsnow, snowgrid, Date)



# clean up camera trap data --------------------------------------------------








# figure and save ---------------------------------------------------------

#figure to show snow depth over winter by year and location
ggplot(snow)+
  geom_point(aes(x = Date, y = SD, color = snowgrid))+
  facet_wrap(~winter, scales = "free")

#save data
saveRDS(snow, "data/snowgrids.rds")
