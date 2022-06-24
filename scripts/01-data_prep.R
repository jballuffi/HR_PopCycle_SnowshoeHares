
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

#variables that we are splitting calculations by
splitburst <- c("id", "winter", "burst")
splityear <- c("id", "winter")

#overwrite id column with animal
gps[, id := animal]

#grab only fixes during winter
gps <- gps[mnth == 11 | mnth == 12 | mnth == 1 | mnth == 2 | mnth == 3]

#remove any fixes not allocated to a burst
gps <- gps[!is.na(burst)]


#categorize fixes into winters
gps[mnth > 9, winter := paste0(yr, "-", yr+1)]
gps[mnth < 4, winter := paste0(yr-1, "-", yr)]

#calculate the difference in days from first day of a burst within a winter
gps[, diffday := idate - min(idate), by = splityear]

#calculate the length of a full collar period per winter
gps[, winterlength := max(idate) - min(idate), by = splityear]

#calculate the length of each burst of collar data
gps[, burstlength := max(idate) - min(idate), by = splitburst]



# lag dates ---------------------------------------------------------------


#calculate the difference between days of fix, in order of datetime, by burst
#bursts with gaps greater than a day or two must be bursts where many fixes got removed in cleaning
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
gps[, next_x_proj := shift(x_proj, n=1, type="lead"), by = .(id, winter, season, burst)]
gps[, next_y_proj := shift(y_proj, n=1, type="lead"), by = .(id, winter, season, burst)]

#calculate step length
gps[, S.L. := sqrt((next_x_proj - x_proj)^2 + (next_y_proj - y_proj)^2)] 

#create speed column
gps[, speed := S.L./fixrate, by= .(id, winter, season, burst)]



# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")







#grab important columns
gps<-gps[, .(id, winter, season, burst, datetime, nextfix, fixrate, x_proj, next_x_proj, y_proj, next_y_proj, S.L.)]

#remove SLs greater than 5000 meters
gps<- gps[S.L. <= 5000]



stats <- gps[, .(median(fixrate), median(S.L.), median(speed)), by = .(id, winter, season, burst)]
setnames(stats, c("V1", "V2", "V3"), c("fixrate", "S.L.", "speed"))


#initial plots
ggplot(gps, aes(S.L., fill = factor(id))) + geom_density(alpha = 0.4) +
   theme(legend.position="none")
hist(gps$S.L.)


