#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in prepped GPS data (just from prep locs)
#create file path to cleaned gps data from the prepare locs project
files <- dir("../prepare-locs-kluane-hares/output/", full.names = TRUE)

#fread the list of files using lapply
ls.files<-lapply(files, fread)

#rbind the list of files
gps <- rbindlist(ls.files, fill = TRUE, use.names = TRUE)

#overwrite id column with animal
gps[, id := animal]

#categorize fixes into winters
gps[mnth > 10, winter := paste0(yr, "-", yr+1)]
gps[mnth < 4, winter := paste0(yr-1, "-", yr)]

#grab only winter
gps <- gps[!is.na(winter)]


#read in sho data
shogps<-fread("Data/liam_test_data/Female Hare GPS locations for Chapter 1.csv")
#convert to date
shogps[, Date := lubridate::mdy_hm(datetime)]
shogps[, mnth := lubridate::month(Date)]
#clip sho's to our winter
shogps<-shogps[mnth > 10 | mnth <4]

# look into IDs
shogps[, uniqueN(ID)] #112 unique hares, he removed 2 to get 110 in paper but he cant remeber why
setnames(shogps, "year", "winter")
shogps[, uniqueN(ID), by=winter] #1 extra in 15/16, 16/17, and 18/19 compared to paper, hopefully just 2 IDs
u112<-shogps[, unique(ID)]

#read in unique IDs used in paper
bid<-fread("Data/liam_test_data/GPS and AXY Monitored Females All Year.csv")
bid[, uniqueN(GPS)] #110 like in paper
u110<- bid[, unique(GPS)]
#no winter data

#read in unique IDs used in paper by winter
idbw<-fread("Data/liam_test_data/Sample_Size_GPS_AXY_Female_All_Years.csv")
idbw[, uniqueN(gps), by=year] #now just 18/19 missing 3 IDs compared to paper
idbw[, uniqueN(gps)] #108 for some reason???
u108 <- idbw[, unique(gps)]

#figure out what hare IDs arenot in paper list
u110 %in% u108 #some F
u110 %in% u112 #All T
bid[GPS %in% u108 == FALSE, unique(GPS)] #25604 25689 30341

u112 %in% u108 #some F
u112 %in% u110 #some F
shogps[ID %in% u108 == FALSE, unique(ID)] #24599  2847 25689 25604 30341
shogps[ID %in% u110 == FALSE, unique(ID)] #24599  2847, latter might be meant to be 22847
a<-shogps[ID==22847] 
b<-shogps[ID==2847] #gap in a is ~ the date range of b, so I think so


u108 %in% u110 #some F
u108 %in% u112 #some F
idbw[gps %in% u110 == FALSE, unique(gps)] #25086
idbw[gps %in% u112 == FALSE, unique(gps)] #25086

#compile list of 6 'problem' IDs
pid<- c(shogps[ID %in% u108 == FALSE, unique(ID)], idbw[gps %in% u110 == FALSE, unique(gps)])
          

#subet our data to shotaro's winters
showinters<-c("2015-2016", "2016-2017", "2017-2018", "2018-2019")
us<-gps[winter %in% showinters]
us[, uniqueN(id), winter]
us[id %in% pid == TRUE, unique(id)] #missing 30341, 25689, and 2847 (last one probably meant to be 22847)


#Get the winters for the file with 110 unique hares
w110<-shogps[, unique(ID), by=winter] 
w110<-w110[V1 != "24599" & V1 != "2847"] #remove teh 2 hares that arent in the 110 file
wid110<-merge(w110, bid, by.x="V1", by.y="GPS", all=T)
setnames(wid110, "V1", "id")
setorder(wid110, winter)
wid110[, uniqueN(id), by=winter] 



#investigating disparrities between Sho and our date ranges------------------------------------------------------
#compare date ranges of bun IDs
usid<-us[, .(min(datetime), max(datetime), .N), by=.(id, winter)]
setnames(usid, c("V1", "V2", "N"), c("us.min.dt", "us.max.dt", "us.N.fixes"))
dr<-shogps[, .(min(Date), max(Date), .N), by=.(ID, winter)]
setnames(dr, c("ID", "V1", "V2", "N"), c("id", "sho.min.dt", "sho.max.dt", "sho.N.fixes"))
mdr<-merge(usid, dr, by=c("id", "winter"), all=T )

#updated may 4
mdr[is.na(us.min.dt), uniqueN(id), by=winter] #30 (used to be 82) IDs by winter sho has that we dont?
mdr[is.na(sho.min.dt), uniqueN(id), by=winter] #39 (used to be 18) IDs by winter we have that sho doesn't
mdr[!is.na(sho.min.dt) & !is.na(us.min.dt), uniqueN(id), by=winter] #117 (used to be 65) IDs by winter we both have
mdr[!is.na(sho.min.dt) & !is.na(us.min.dt), fix.diff.N := abs(us.N.fixes - sho.N.fixes), by=winter] 

us[, uniqueN(id), winter] #but the mtching is stil not great for 2018-2019 (45 for sho vs 28 for us)
shogps[, uniqueN(ID), winter]

#but when we share IDs in a winter, the number of fixes are usually very different
# export this
write.csv(mdr, "data/liam_test_data/haregps_comparingtoSho.csv")




#  Exploring hare data Sho has that we don't ----------------------------------------------------------------

nio<-dr[id %notin% usid$id ] #sho  has 66  ids not in ours

#several may be truncated by oct and april data but should still exist in ours bc nothing is just in those months
    #clarify what our date range is and why?
#should be able to take this data and ask Alec for help on why theyre getting weeded out by cleaning?
#get list of possible options/issues first (go thru cleaning script)



#  Exploring hare data we have that sho doesnt ----------------------------------------------------------

(nis<-usid[id %notin% dr$id]) #we have 18 ids not in sho's

#how many of these are in sho's list of 14 extra hares (no axy, male)
(nis2<-nis[id %notin% bid$gps_extra]) #3, so we really have 15 IDs not in Sho's
#how many are males? 
sid<-readRDS("Data/liam_test_data/sexandID_our_data")
s<-sid[winter %in% showinters]
s<-s[, .(Sex, id)]
m<-s[Sex==1] 
f<-s[Sex==2] 
shogps[ID %in% m$id, unique(ID)] #24599 seems to be male
(nis3<-nis2[id %notin% m$id]) #13 of 15 not in sho's are male
#nis3 (25086, 22113) are the 2 hares that arent in Sho's but are in ours, unclear why (25086 is in one of his sheets, just not hte actual data)
#send these to sho along with other issues in excel
z<-us[id == "22847"]
