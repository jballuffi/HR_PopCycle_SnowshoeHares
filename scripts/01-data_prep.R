
# script that imports cleaned GPS data from the prepare-locs scripted (Authored by Alec Robitaille)
# and prepares data for home range size analysis


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# collect files  -----------------------------------------------------------

#create file path to cleaned gps data from the prepare locs project
files <- dir("../prepare-locs-kluane-hares/output/", full.names = TRUE)

#fread the list of files using lapply
ls.files<-lapply(files, fread)

#rbind the list of files
gps <- rbindlist(ls.files, fill = TRUE, use.names = TRUE)

#read in trapping data (to get grid)
trapping <- fread("data/Trapping_data_all_records.csv")




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



# merge with trapping data to get grid ------------------------------------

#pull out the grid of every individual using the trapping data
grids <- trapping[, getmode(grid), Eartag]
names(grids) <- c("id", "grid")

#merge grids into behaviour data set
gps <- merge(gps, grids, by = "id", all.x = TRUE)


# calculate time differences in data and sample periods ------------------------------------------------

#calculate the difference in days from first day of a winter
gps[, diffday := idate - min(idate), by = deploy_id]
gps[, max(diffday)]

#cut diffday into weeks
gps[, week := cut(diffday, breaks = seq(-1, 200, by = 7))] #use 200 bc far above max diff day w/ or w/o deployID

#calculate how many days are in each week using number of unique dates
gps[, weeklength := length(unique(idate)), by = .(deploy_id, week)]

#take only weeks that have over 6 days of sampling
gps <- gps[weeklength > 6] #174496 w/o deployid; 181676 w deployid

#get number of full weeks per deployid
gps[, n.hare.weeks := uniqueN(week), by=deploy_id]
#how many full weeks total
nw<- gps[, unique(n.hare.weeks), by=deploy_id]
nw[, sum(V1)] #637 hare weeks w/o deployid; 654 hare weeks w/ deployid

#average the date for each week
gps[, weekdate := mean(idate), by = .(id, winter, week)]

#create split week
splitweek <- c("id", "winter", "week")


# fix rates, step length and speed ---------------------------------------------------------------
setorder(gps, datetime)

gps[, prevfix := shift(datetime, n = 1, type = "lag"), by = splitweek] #take time before , for each fix
gps[, difffix := as.numeric(round(difftime(datetime, prevfix, units = 'mins')))] #calculate difference between previous time and current time
gps[difffix < 100, hist(difffix)]
#a lot of zero fix rates - they should have been removed in prep_locs?

#put previous coordinates in new column
setorder(gps, datetime)
gps[, prev_x_proj := shift(x_proj, n = 1, type = "lag"), by = splitweek]
gps[, prev_y_proj := shift(y_proj, n = 1, type = "lag"), by = splitweek]

#calculate step length
gps[, sl := sqrt((prev_x_proj - x_proj)^2 + (prev_y_proj - y_proj)^2)] 

#create speed column
gps[, speed := sl/difffix, by = splitweek]


# clean out unrealistic movements -----------------------------------------

#remove outlier fixes based on distribution of easting and northings
#eastings
gps <- gps[x_proj > 300000 & x_proj < 342000] #removed 10
#northings
gps <- gps[y_proj > 6750000 & y_proj < 6770000] #removed 2 more
#check why one mean y_lat became 0 when just y_lat was non-zero???


#remove cases where fix rate is zero minutes
gps <- gps[!difffix == 0]

#define the upper quantile of speed
quant <- quantile(gps$speed, probs = 0.995, na.rm = TRUE)
#remove remove speeds greater than the 99.5% percentile
gps<- gps[speed <= quant]


#add cols for median fixrate, mode fixrate, median steplength, median speed
gps[, med.fixrate := median(difffix), by = splitweek]
gps[, modeFR := getmode(difffix), by = splitweek]
gps[, med.sl := median(sl), by = splitweek]
gps[, med.speed := median(speed), by = splitweek]


#Compare median and mode fixdiff to determine real fix rates
gps[, unique(modeFR), by = med.fixrate]

#looked into individual buns, and mode seems to better estimate true fix rate
#so, resetting fix rate as the mode
gps[, fixrate := modeFR]


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")

