
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


# Reduce and categorize data -----------------------------------------------------

#variables that we are splitting calculations by
splitburst <- c("id", "winter", "burst")
splityear <- c("id", "winter")

#overwrite id column with animal
gps[, id := animal]

#categorize fixes into winters
gps[mnth > 10, winter := paste0(yr, "-", yr+1)]
gps[mnth < 4, winter := paste0(yr-1, "-", yr)]

#grab only winter
gps <- gps[!is.na(winter)]

#remove any fixes not allocated to a burst
gps <- gps[!is.na(burst)]



# calculate sample periods ------------------------------------------------

#calculate the difference in days from first day of a burst
gps[, diffday := idate - min(idate), by = splitburst]

#calculate the length of each burst
gps[, burstlength := max(idate) - min(idate), by = splitburst]


# lag dates ---------------------------------------------------------------


#calculate the difference between days of fix, in order of datetime, by burst
#bursts with gaps greater than a day or two must be bursts where many fixes got removed in cleaning
#calculate the difference between days of fix, in order of datetime, by season
setorder(gps, datetime)

#calculate the difference between times of fix, in order of datetime, by season
setorder(gps, datetime)
gps[, shiftdate := shift(idate), by = splitburst] #take date before , for each fix
gps[, lagdate := idate - shiftdate] #calculate difference between previous date and current date
#take max lag between fixes for each bunny-winter
gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = splityear]



# fix rates, step length and speed ---------------------------------------------------------------

gps[, nextfix := shift(datetime, n=1, type="lead"), by = splitburst] #take time before , for each fix
gps[, difffix := as.numeric(round(difftime(nextfix, datetime, units= 'mins')))] #calculate difference between previous time and current time
hist(gps$difffix)
#a lot of zero fix rates - they should have been removed in prep_locs?

#put next coordinates in new column
setorder(gps, datetime)
gps[, next_x_proj := shift(x_proj, n=1, type="lead"), by = splitburst]
gps[, next_y_proj := shift(y_proj, n=1, type="lead"), by = splitburst]

#calculate step length
gps[, sl := sqrt((next_x_proj - x_proj)^2 + (next_y_proj - y_proj)^2)] 

#create speed column
gps[, speed := sl/difffix, by = splitburst]


# clean out unrealistic movements -----------------------------------------


#remove cases where fix rate is zero minutes
gps <- gps[!difffix == 0]

#define the upper quantile of speed
quant <- quantile(gps$speed, probs = 0.995, na.rm = TRUE)
#remove remove speeds greater than the 99.5% percentile
gps<- gps[speed <= quant]

#remove fix rates over 12 hours (just anything unreasonable)
gps <- gps[difffix <= 720]


#add cols for median fixrate, median steplength, median speed
gps[, fixrate := median(difffix), by = splitburst]
gps[, med.sl := median(sl), by = splitburst]
gps[, med.speed := median(speed), by = splitburst]



# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")

