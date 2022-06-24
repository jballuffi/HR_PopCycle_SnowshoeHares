
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

#create early winter and late winter categories
#fixes in nov and dec are "early"
#fixes in feb and march are "late"
gps[mnth == 11| mnth == 12, season := "early"]
gps[mnth == 2| mnth == 3, season := "late"]

#remove anything that doesn't fall into early or late winter
gps <- gps[!is.na(season)]

#variables that we are splitting calculations by
splitburst <- c("id", "winter", "burst")
splitseason <- c("id", "winter", "season")

#calculate the difference in days from first day of a burst
gps[, diffday := idate - min(idate), by = splitburst]

#calculate the length of a season
gps[, seasonlength := max(idate) - min(idate), by = splitburst]

#calculate the length of each burst
gps[, burstlength := max(idate) - min(idate), by = splitburst]

#calculate the difference between days of fix, in order of datetime, by season
setorder(gps, datetime)
gps[, shiftdate := shift(idate), by = splitseason] #take date before , for each fix
gps[, lagdate := idate - shiftdate] #calculate difference between previous date and current date

#take max lag between fixes for each bunny-season
gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = splitseason]


#Liam Step length analysis --------------------------------------------------

#use only sample periods that are 10 days or more
gps <- gps[burstlength >= 10]

gps <- gps[!is.na(burst)]

#calculate the difference between times of fix, in order of datetime, by season
setorder(gps, datetime)
gps[, nextfix := shift(datetime, n=1, type="lead"), by = .(id, winter, season, burst)] #take time before , for each fix
gps[, fixrate := as.numeric(round(difftime(nextfix, datetime, units= 'mins')))] #calculate difference between previous time and current time
hist(gps$fixrate)
#a lot of zero fix rates - they should have been removed in prep_locs?

#put next coordinates in new column
setorder(gps, datetime)
gps[, next_x_proj := shift(x_proj, n=1, type="lead"), by = .(id, winter, season, burst)]
gps[, next_y_proj := shift(y_proj, n=1, type="lead"), by = .(id, winter, season, burst)]

#calculate step length
gps[, S.L. := sqrt((next_x_proj - x_proj)^2 + (next_y_proj - y_proj)^2)] 

#grab important columns - ignore with '#' for purpose of creating full output
#gps<-gps[, .(id, winter, season, burst, datetime, nextfix, fixrate, x_proj, next_x_proj, y_proj, next_y_proj, S.L.)]


#create new columns for speed, 
gps[, speed := S.L./fixrate, by= .(id, winter, season, burst)]

#remove SLs greater than 5000 meters
gps<- gps[S.L. <= 5000] #below stats functions were not working unless I ran this line
#It must have removed some NAs?????????????????????????

#add cols for median fixrate, median steplength, median speed
gps[, median.fixrate := median(fixrate), by= .(id, winter, season, burst)]
gps[, median.SL := median(S.L.), by= .(id, winter, season, burst)]
gps[, median.speed := median(speed), by= .(id, winter, season, burst)]

quantile(gps$speed, probs=0.987) # this was percentile where it seemed reasonable
#remove remove speeds greater than the 98.7% percentile (> 194 meters/min)
gps<- gps[speed <= 194]
#remove fix rates over 12 hours (just anything unreasonable)
gps <- gps[fixrate <= 720]
#remove fix rates = 0 (shouldnt be there after speed reduction (infinity), but just check)
gps<- gps[fixrate != 0]
#remove fixes wth step lengths above 99.7% quantile (balance data reduction and reasonable SL)
quantile(gps$S.L., probs=0.997)
gps<- gps[S.L. <= 500]

#initial plots
ggplot(gps, aes(S.L., fill = factor(id))) + geom_density(alpha = 0.4) +
   theme(legend.position="none")
hist(gps$S.L.)


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")
