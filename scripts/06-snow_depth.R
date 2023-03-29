
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


#data from transect measures
sd1 <- fread("data/Kluane_PredTrans_SnowDepth.csv")
sd2 <- fread("data/snowdepthdataentry.csv")



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



# clean up transect data --------------------------------------------------

#rename cols
names(sd2) <- c("where", "Date", "SD", "Comments")

#make date a date
sd2[, Date := dmy(Date)]

#clear old data out
sd2 <- sd2[Date >= "2015-11-01"]

#remove comments
sd2[, Comments := NULL]

#define the last date in Alice's data
enddate <- sd2[, max(Date)]

#add  label for transect data (just the segment because that is a unique measure and
#tells that it is from pred transect data)
sd1[, where := Segment]

#convert to date
sd1[, Date := dmy(DateTrans)]

#remove data from before November 1, 2015 (the cutoff date for HR analysis)
sd1 <- sd1[Date >= "2015-11-01"]

#remove NA snow depths 
sd1[!is.na(SnowDepthStn), unique(year(Date))] #(ask Alice about no data 2017-19) 
sd1 <- sd1[!is.na(SnowDepthStn)]

#remove repeat snow measure per segment
sd1 <- sd1[, unique(SnowDepthStn), by = .(Date, where)]
setnames(sd1, "V1", "SD")

#subset to only dates after SD2 ends
sd1late <- sd1[Date > enddate]

#merge two dts together and remove faulty values (36 where sdepth = -9999)
tsnow <- rbind(sd2, sd1late)
tsnow <- tsnow[!SD < 0]

#avg across locations by date
tsnow <- tsnow[, mean(SD), by = Date]
setnames(tsnow, "V1", "SD")

#create a grid column
tsnow[, snowgrid := "Transect"]


# Test correlation between transect data and grid data --------------------

#take mean snow depth across all grids by date
gridmean <- gsnow[, mean(SD), Date]
setnames(gridmean, "V1", "gridSD")

#merge with transect data by date
datatest <- merge(gridmean, tsnow, by = "Date", all.y = TRUE)

ggplot(datatest)+
  geom_point(aes(x = SD, y = gridSD))



# final merge of transect and grid data -----------------------------------


snow <- rbind(gsnow, tsnow)

#categorize fixes into winters
snow[month(Date) > 10, winter := paste0(year(Date), "-", year(Date) + 1)]
snow[month(Date) < 4, winter := paste0(year(Date) - 1, "-", year(Date))]
snow <- snow[!is.na(winter)]

#figure to show snow depth over winter by year and location
ggplot(snow)+
  geom_point(aes(x = Date, y = SD, color = snowgrid))+
  facet_wrap(~winter, scales = "free")

#save data
saveRDS(snow, "data/snowgrids.rds")
