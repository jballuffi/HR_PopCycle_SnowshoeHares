
# script that prepares the *already cleaned* gps data for all aspects of analysis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# collect files  -----------------------------------------------------------

#create file path to cleaned gps data from the prepare locs project
files <- dir("../prepare-locs-kluane-hares/output/", full.names = TRUE)


#fread the list of files using lapply
ls.files<-lapply(files, fread)

#rbind the list of files
gps <- rbindlist(ls.files, fill = TRUE, use.names = TRUE)


# Reconstruct data -----------------------------------------------------

#overwrite id column with animal
gps[, id := animal]

#categorize gps fixes into winters
gps[idate > "2015-10-31" & idate < "2016-04-01", winter := "2015-2016"]
gps[idate > "2016-10-31" & idate < "2017-04-01", winter := "2016-2017"]
gps[idate > "2017-10-31" & idate < "2018-04-01", winter := "2017-2018"]
gps[idate > "2018-10-31" & idate < "2019-04-01", winter := "2018-2019"]
gps[idate > "2019-10-31" & idate < "2020-04-01", winter := "2019-2020"]
gps[idate > "2020-10-31" & idate < "2021-04-01", winter := "2020-2021"]
gps[idate > "2021-10-31" & idate < "2022-04-01", winter := "2021-2022"]

#remove anything that doesn't fall into winter
gps <- gps[!is.na(winter)]

#create early winter and late winter categories
#fixes in nov and dec are "early"
#fixes in feb and march are "late"
gps[mnth == 11| mnth == 12, season := "early"]
gps[mnth == 2| mnth == 3, season := "late"]

#variables that we are splitting calculations by
splitburst <- c("id", "winter", "burst")
splitseason <- c("id", "winter", "season")

#calculate the difference in days from first day of a burst
gps[, diffday := idate - min(idate), by = splitburst]

#calculate the total sample range per bunny-burst
gps[, burstlength := max(diffday), by = splitburst]

#calculate the total sample range per bunny-season
gps[, seasonlength := max(idate - min(idate)), by = splitseason]

#calculate the difference between days of fix, in order of datetime
setorder(gps, datetime)
gps[, shiftdate := shift(idate), by = splitby]
gps[, lagdate := idate - shiftdate]

#take max lag between fixes for each bunny-season
gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = splitby]


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")
