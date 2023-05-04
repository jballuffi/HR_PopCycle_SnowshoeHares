
# script that imports cleaned GPS data from the prepare-locs scripted (Authored by Alec Robitaille)
# and prepares data for home range size analysis
#authors: Juliana Balluffi-Fry and Liam Horne

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

#If we want to run something by id and winter only. We use to use this but switched to using deploy_id
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

#calculate the difference in days from first day of a deployment
gps[, diffday := idate - min(idate), by = deploy_id]
gps[, maxdiffday := max(diffday), by = deploy_id]

#cut diffday into weeks
gps[, week := cut(diffday, breaks = seq(-1, 200, by = 7))] #use 200 bc far above max diff day w/ or w/o deployID

#calculate how many days are in each week using number of unique dates
gps[, weeklength := length(unique(idate)), by = .(deploy_id, week)]

#take only weeks that have over 6 days of sampling
gps <- gps[weeklength > 6] #174496 w/o deployid; 181676 w deployid

#get number of full weeks per deployid
gps[, n.hare.weeks := uniqueN(week), by = deploy_id]

#how many full weeks total
nw <- gps[, unique(n.hare.weeks), by = deploy_id]
nw[, sum(V1)] #637 hare weeks w/o deployid; 654 hare weeks w/ deployid

#average the date for each week
gps[, weekdate := mean(idate), by = .(deploy_id, week)]

#create split week
splitweek <- c("id", "winter", "weekdate")


# calculate fix rates, step length and speed ---------------------------------------------------------------
setorder(gps, datetime)

gps[, prevfix := shift(datetime, n = 1, type = "lag"), by = splitweek] #take time before , for each fix
gps[, difffix := as.numeric(round(difftime(datetime, prevfix, units = 'mins')))] #calculate difference between previous time and current time
gps[difffix < 100, hist(difffix)]
gps[, n.fixes := nrow(.SD), .(week, deploy_id)]
gps[, hist(n.fixes)]
#a lot of zero fix rates - they should have been removed in prep_locs?

#put previous coordinates in new column
setorder(gps, datetime)
gps[, prev_x_proj := shift(x_proj, n = 1, type = "lag"), by = splitweek]
gps[, prev_y_proj := shift(y_proj, n = 1, type = "lag"), by = splitweek]

#calculate step length
gps[, sl := sqrt((prev_x_proj - x_proj)^2 + (prev_y_proj - y_proj)^2)] 

#create speed column
gps[, speed := sl/difffix]


# clean out unrealistic movements -----------------------------------------

#remove remove speeds greater than 750 meter/min (top speed is 45kph)
gps <- gps[speed <= 750 | is.na(speed) | speed == Inf]

#add cols for median fixrate, mode fixrate, median steplength, median speed
gps[!is.na(difffix), med.fixrate := median(difffix), by = splitweek]
gps[!is.na(difffix), modeFR := getmode(difffix), by = splitweek]
gps[!is.na(sl), med.sl := median(sl), by = splitweek]
gps[!is.na(speed) & speed != Inf, med.speed := median(speed), by = splitweek]


#Compare median and mode fixdiff to determine real fix rates
gps[, unique(modeFR), by = med.fixrate]

#looked into individual buns, and mode seems to better estimate true fix rate
#so, resetting fix rate as the mode
gps[, fixrate := modeFR]


# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")

