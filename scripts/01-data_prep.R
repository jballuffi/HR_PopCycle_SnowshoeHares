
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

#get rid of year column
gps[, year := NULL]

#create a date column
gps[, date := tstrsplit(datetime, " ", keep = 1)][, date := mdy(date)]

#categorize gps fixes into winters
gps[date > "2015-10-01" & date < "2016-04-01", winter := "2015-2016"]
gps[date > "2016-10-01" & date < "2017-04-01", winter := "2016-2017"]
gps[date > "2017-10-01" & date < "2018-04-01", winter := "2017-2018"]
gps[date > "2018-10-01" & date < "2019-04-01", winter := "2018-2019"]

#remove anything that doesn't fall into winter
gps <- gps[!is.na(winter)]

#calculate the difference in days from first day of sampling in winter
gps[, diffday := date - min(date), by = c("ID", "winter")]
