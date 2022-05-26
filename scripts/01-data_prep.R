
# script that prepares the *already cleaned* gps data for all aspects of analysis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# collect files  -----------------------------------------------------------

#create file path to cleaned gps data
files <- dir("../prepare-locs-kluane-hares/output/", full.names = TRUE)


#lapply a little function to fread the list of files
ls.files<-lapply(files, fread)

#rbind the list of files
gps <- rbindlist(ls.files, fill = TRUE, use.names = TRUE)


# Reconstructure data -----------------------------------------------------

#categorize gps fixes into winters
gps[idate > "2015-10-31" & idate < "2016-04-01", winter := "2015-2016"]
gps[idate > "2016-10-31" & idate < "2017-04-01", winter := "2016-2017"]
gps[idate > "2017-10-31" & idate < "2018-04-01", winter := "2017-2018"]
gps[idate > "2018-10-31" & idate < "2019-04-01", winter := "2018-2019"]

#remove anything that doesn't fall into winter
gps <- gps[!is.na(winter)]

#remove anything from January
gps <- gps[mnth != 1]

#create early winter and late winter categories
#fixes in nov and dec are "early"
#fixes in feb and march are "late"
gps[mnth == 11| mnth == 12, season := "early"]
gps[mnth == 2| mnth == 3, season := "late"]


#variables that we are splitting calculations by
splitby <- c("id", "winter", "season")

#calculate the difference in days from first day of sampling in winter
gps[, diffday := idate - min(idate), by = splitby]

#calculate the total sample range per bunny-season (range of first fix to last fix)
gps[, samplerange := max(diffday), by = splitby]

#calculate number of unique dates per bunny-season
gps[, uniquedays := uniqueN(idate), by = splitby]

#calculate the difference between days of fix, in order of datetime
setorder(gps, datetime)
gps[, shiftdate := shift(idate), by = splitby]
gps[, lagdate := idate - shiftdate]

#take max lag between fixes for each bunny-season
gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = splitby]


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")
