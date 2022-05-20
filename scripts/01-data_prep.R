
# script that prepares the *already cleaned* gps data for all aspects of analysis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# collect files  -----------------------------------------------------------

#create file path to cleaned gps data
files <- dir("data/Cleaned_gps", full.names = TRUE)


#lapply a little function to fread the list of files
ls.files<-lapply(files, FUN=function(m){
  dt<-fread(m)
  return(dt)
})

#rbind the list of files
gps <- rbindlist(ls.files, fill = TRUE, use.names = TRUE)




# Reconstructure data -----------------------------------------------------

#make ID a factor
gps[, ID := as.factor(ID)]

#make datetime a datetime
gps[, datetime := as.POSIXct(datetime, format = '%m/%d/%Y %H:%M')]

#get rid of year column
gps[, year := NULL]

#create a date column
gps[, date := as.Date(datetime)]

#categorize gps fixes into winters
gps[date > "2015-10-31" & date < "2016-04-01", winter := "2015-2016"]
gps[date > "2016-10-31" & date < "2017-04-01", winter := "2016-2017"]
gps[date > "2017-10-31" & date < "2018-04-01", winter := "2017-2018"]
gps[date > "2018-10-31" & date < "2019-04-01", winter := "2018-2019"]

#remove anything that doesn't fall into winter
gps <- gps[!is.na(winter)]

#take out month from date column
gps[, m := month(date)]

#remove anything from January
gps <- gps[!m == 1]

#create early winter and late winter categories
#fixes in nov and dec are "early"
#fixes in feb and march are "late"
gps[m == 11| m == 12, season := "early"]
gps[m == 2| m == 3, season := "late"]


#calculate the difference in days from first day of sampling in winter
gps[, diffday := date - min(date), by = c("ID", "winter", "season")]

#calculate the total sample range per bunny-season (range of first fix to last fix)
gps[, samplerange := max(diffday), by = c("ID", "winter", "season")]

#calculate number of unique dates per bunny-season
gps[, uniquedays := uniqueN(date), by = c("ID", "winter", "season")]

#calculate the difference between days of fix, in order of datetime
setorder(gps, datetime)
gps[, shiftdate := shift(date), by = c("ID", "winter", "season")]
gps[, lagdate := date - shiftdate]

#take max lag between fixes for each bunny-season
gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = c("ID", "winter", "season")]


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")
