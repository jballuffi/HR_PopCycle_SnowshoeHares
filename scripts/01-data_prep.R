
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

#import food add bunnies
foodadd <- readRDS("data/food_adds.rds")


# Reduce and categorize data -----------------------------------------------------

#If we want to run something by id and winter only. We use to use this but switched to using deploy_id
splityear <- c("id", "winter")

#overwrite id column with animal
gps[, id := animal]

#categorize fixes into winters
gps[mnth > 11, winter := paste0(yr, "-", yr+1)]
gps[mnth < 4, winter := paste0(yr-1, "-", yr)]

#merge in food add
setnames(foodadd, "Eartag", "id")
gps <- merge(gps, foodadd, by = c("id", "winter"), all.x = TRUE)
gps[is.na(Food), Food := 0]

#grab only winter
gps <- gps[!is.na(winter)]


# merge with trapping data to get grid ------------------------------------

#pull out the grid of every individual using the trapping data
grids <- trapping[, getmode(grid), Eartag]
names(grids) <- c("id", "grid")

#merge grids into behaviour data set
gps <- merge(gps, grids, by = "id", all.x = TRUE)

gps <- gps[Food == 1 & winter == "2016-2017"]

# calculate time differences in data and sample periods ------------------------------------------------

#calculate winter day, which is how many days since dec 1 (doy = 335)
gps[doy > 334, winterday := doy - 335] #if between nov 1 and dec 31, just subtract from nov 1
gps[doy < 334, winterday := doy + 61] #if after jan 1, add doy to 61 (dec 31 - nov 1)

#calculate the difference in days from first day of a deployment
gps[, diffday := idate - min(idate) + 1, by = .(id, winter)]
gps[, maxdiffday := max(diffday), by = .(id, winter)]

#cut diffday into weeks
gps[, week := cut(diffday, breaks = seq(0, 200, by = 7))] #use 200 bc far above max diff day w/ or w/o deployID

#calculate how many days are in each week using number of unique dates
gps[, weeklength := length(unique(idate)), by = .(id, week)]

#take only weeks that have over 6 days of sampling
gps <- gps[weeklength > 6] #174496 w/o deployid; 181676 w deployid

#get number of full weeks per deployid
gps[, n.hare.weeks := uniqueN(week), by = deploy_id]

#how many full weeks total
nw <- gps[, unique(n.hare.weeks), by = deploy_id]
nw[, sum(V1)] #637 hare weeks w/o deployid; 654 hare weeks w/ deployid

#average the date for each week
gps[, weekdate := mean(idate), by = .(deploy_id, week)]

#average the winter day for each week
gps[, weekwinterday := mean(winterday), by = .(deploy_id, week)]

#Correct deploy_ids that are clearly two collars ------------------------
#get list of days per deploy_id
s<-gps[, unique(idate), deploy_id]
setnames(s, "V1", "u.idate")

#check for large gaps between days
setorder(s, u.idate)
s[, prev.date := shift(u.idate, n = 1, type = "lag"), by = deploy_id] 
s[, dd := u.idate-prev.date]
ss<-s[dd>1] #get gaps larger than 1 day
di<-s[dd>1, deploy_id] #get deploy ids for these gaps
dd<-s[dd>1, u.idate] #get the first date after a large gap

qw<-gps[deploy_id %in% di] #get  deploy ids that have large gaps
gps<-gps[deploy_id %notin% di] #temporarily remove these fixes with these deploy ids from gps
q<-merge(qw, ss, by="deploy_id") #add the post-gap date to main data
q[idate >= u.idate, deploy_id := paste0(id,"_",u.idate)] #make new deployment id when date is after the gap
q2<-q[, c("u.idate", "dd", "prev.date") := NULL] #reduce cols for rbind
gps<-rbind(gps, q2) #added 4 deploy IDs


#RE-CALCULATE the difference in days from first day of a deployment
gps[, diffday := idate - min(idate), by = deploy_id]
#cut diffday into weeks
gps[, week := cut(diffday, breaks = seq(-1, 200, by = 7))] 
#calculate how many days are in each week using number of unique dates
gps[, weeklength := length(unique(idate)), by = .(deploy_id, week)]
#take only weeks that have over 6 days of sampling
gps <- gps[weeklength > 6] #No change
#get number of full weeks per deployid
gps[, n.hare.weeks := uniqueN(week), by = deploy_id]
#how many full weeks total
nw <- gps[, unique(n.hare.weeks), by = deploy_id]
nw[, sum(V1)] #No Change


# calculate fix rates, step length and speed ---------------------------------------------------------
#create split week
splitweek <- c("id", "winter", "weekdate")

setorder(gps, datetime)

gps[, prevfix := shift(datetime, n = 1, type = "lag"), by = deploy_id] #take time before , for each fix
gps[, difffix := as.numeric(round(difftime(datetime, prevfix, units = 'mins')))] #calculate difference between previous time and current time
gps[difffix < 100, hist(difffix)]
gps[, n.fixes := nrow(.SD), .(week, deploy_id)]
gps[, hist(n.fixes)]
#a lot of zero fix rates - they should have been removed in prep_locs?

#put previous coordinates in new column
setorder(gps, datetime)
gps[, prev_x_proj := shift(x_proj, n = 1, type = "lag"), by = deploy_id]
gps[, prev_y_proj := shift(y_proj, n = 1, type = "lag"), by = deploy_id]

#calculate step length
gps[, sl := sqrt((prev_x_proj - x_proj)^2 + (prev_y_proj - y_proj)^2)] 

#create speed column
gps[, speed := sl/difffix]


# clean out unrealistic movements -----------------------------------------

#remove remove speeds greater than 750 meter/min (top speed is 45kph)
gps <- gps[speed <= 750 | is.na(speed) | speed == Inf] #181676 - 181274 = removed 402 fixes

#add cols for median fixrate, mode fixrate, median steplength, median speed
gps[!is.na(difffix), med.fixrate := median(difffix), by = deploy_id]
gps[!is.na(difffix), modeFR := getmode(difffix), by = deploy_id]
gps[!is.na(sl), med.sl := median(sl), by = deploy_id]
gps[!is.na(speed) & speed != Inf, med.speed := median(speed), by = deploy_id]


#Compare median and mode fixdiff to determine real fix rates
gps[, unique(modeFR), by = med.fixrate]

#looked into individual buns, and mode seems to better estimate true fix rate
#so, resetting fix rate as the mode
gps[, fixrate := modeFR]

#fill in fixrate NAs by deploy_id (fix rate should be constant per deploy id)
fr<-gps[!is.na(fixrate), unique(fixrate), deploy_id]
c<-merge(gps, fr, by="deploy_id", all.x=T)
gps<-c[, fixrate := V1]
gps[, V1 := NULL]



# make factors ------------------------------------------------------------

gps[, id := as.factor(id)]
gps[, Food := as.factor(Food)]



# Save compiled gps data --------------------------------------------------
saveRDS(gps, "Data/all_gps.rds")

