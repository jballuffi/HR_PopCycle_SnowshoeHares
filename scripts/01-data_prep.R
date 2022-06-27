
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
splitwinter <- c("id", "winter")

#overwrite id column with animal
gps[, id := animal]

#categorize fixes into winters
gps[mnth > 10, winter := paste0(yr, "-", yr+1)]
gps[mnth < 4, winter := paste0(yr-1, "-", yr)]

#grab only winter
gps <- gps[mnth > 10| mnth < 4]

#remove any fixes not allocated to a burst
gps <- gps[!is.na(burst)]



# calculate sample periods ------------------------------------------------

#calculate the difference in days from first day of a burst
gps[, diffday := idate - min(idate), by = splitburst]

#calculate the length of a season
gps[, winterlength := max(idate) - min(idate), by = splityear]

#calculate the length of each burst
gps[, burstlength := max(idate) - min(idate), by = splitburst]

#calculate the difference between days of fix, in order of datetime, by season
setorder(gps, datetime)
# gps[, shiftdate := shift(idate), by = splityear] #take date before , for each fix
# gps[, lagdate := idate - shiftdate] #calculate difference between previous date and current date
# 
# #take max lag between fixes for each bunny-season
# gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = splityear]



# lag dates ---------------------------------------------------------------


#calculate the difference between days of fix, in order of datetime, by burst
#bursts with gaps greater than a day or two must be bursts where many fixes got removed in cleaning

#calculate the difference between times of fix, in order of datetime, by season

setorder(gps, datetime)
gps[, shiftdate := shift(idate), by = splitburst] #take date before , for each fix
gps[, lagdate := idate - shiftdate] #calculate difference between previous date and current date
#take max lag between fixes for each bunny-winter
gps[, maxlagdate := max(lagdate, na.rm = TRUE), by = splitburst]



# fix rates, step length and speed ---------------------------------------------------------------

gps[, nextfix := shift(datetime, n=1, type="lead"), by = splitburst] #take time before , for each fix
gps[, fixrate := as.numeric(round(difftime(nextfix, datetime, units= 'mins')))] #calculate difference between previous time and current time
hist(gps$fixrate)
#a lot of zero fix rates - they should have been removed in prep_locs?

#put next coordinates in new column
setorder(gps, datetime)
gps[, next_x_proj := shift(x_proj, n=1, type="lead"), by = splitburst]
gps[, next_y_proj := shift(y_proj, n=1, type="lead"), by = splitburst]

#calculate step length
gps[, S.L. := sqrt((next_x_proj - x_proj)^2 + (next_y_proj - y_proj)^2)] 

#create speed column
gps[, speed := S.L./fixrate, by = splitburst]



# clean out unrealistic movements -----------------------------------------


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

#add cols for median fixrate, median steplength, median speed
gps[, median.fixrate := median(fixrate), by = splitburst]
gps[, median.SL := median(S.L.), by = splitburst]
gps[, median.speed := median(speed), by = splitburst]



# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")









#initial plots
ggplot(gps, aes(S.L., fill = factor(id))) + geom_density(alpha = 0.4) +
   theme(legend.position="none")
hist(gps$S.L.)


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")
