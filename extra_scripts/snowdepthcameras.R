#Getting the Wildtrax csv subsetted to download the images for snow depth recording
#creating a data sheet template for snow depth recording

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#Prep---------------------------------------------------------------
#read in snow camera data from WildTrax
sd<-fread("Data/liam_test_data/BOUTIN_Kluane_image_report.csv")
#only keep timelapse photos
sd<-sd[trigger == "Time Lapse"]
#get the julian day
sd[, jday := lubridate::yday(date_detected)]
#get the year
sd[, yr := lubridate::year(date_detected)]

#subset dates
#remove before winter 2019-20
sd<-sd[date_detected >= "2019-10-01 00:00:00"]
#keep only oct 1-April 30 (<=121 >=275) ( leap year)
#keep only oct 1-April 30 (<=120 >=274) (non leap year)
sd1<-sd[(yr == 2020 & jday <= 121) | (yr != 2020 & jday <= 120)]
sd2<-sd[(yr == 2020 & jday >= 275) | (yr != 2020 & jday >= 274)]
sd3<-rbind(sd1, sd2) #bind together

#get a col just date and just time
sd3[, date := tstrsplit(date_detected, " ", keep=1)]
sd3[, time := tstrsplit(date_detected, " ", keep=2)]

#subset to just cameras of interest
locs<-c("CH", "B11_1", "A10_4", "A9_3", "B9", "SU", "B8_1", "A7_5")
sd3r<- sd3[location %in% locs]

#Check for camera-days with multi images and keep only one image-------------------------------------------------

#check whether only 1 image per date
a<-sd3r[, .N, .(date, location)]
b<-a[N >1] #get the locs where there were multiple images on a day
b[, .N, location] #only 4 locations had multi images per day- CH and SU were taking pics at 1100 and 1200,
#B8_1 has true duplicate images, but only on the csv not wildtrax, and B11_1 see below

#there are pics from 2 different locations at start of B11_1 (oct 1-17 2019)!
b11<-sd3r[location=="B11_1" & yr <= 2019 & jday < 291] #17 *2 pics =34

#deal with B11_1 separately so the correct images are deleted.
#create col for future  id so images from same day don't overwrite
sd3r[location=="B11_1" & yr <= 2019 & jday < 291, addon := rep(1:2, length.out=.N)] 
di.b11<-sd3r[location=="B11_1"] #for download images function
di.b11[!is.na(addon), date := paste0(date, "_", addon)]

#remove duplicate days (only need one snowdepth per day (ideally at 1100)
#BUT DONT remove dups for B11_1 bc some images from the wrong location will be kept (have to deal with these manually)
sd3r[location != "B11_1", dup := duplicated(date), by=location] 
#remove duplicates and B11_1 data (which is NA)
nodup<-sd3r[dup == FALSE] #this conveniently keeps the 11:00 not 12:00 images

# Prep data for function and run function -------------------------------------------
setnames(di.b11, "image_url(admin only)",  "image_url")
setnames(di.b11, "date",  "image_id") 

setnames(nodup, "image_url(admin only)",  "image_url")
setnames(nodup, "date",  "image_id") 

# test<-sd3r[c(1, 236:239, 340:342)]

#run the function
#(Will download ALL B11_1  photos, and will then just ignore the 17 from the wrong location)
download.images(dat=di.b11, file.path = "../../../Desktop/snowdepth/")
download.images(dat=nodup, file.path = "../../../Desktop/snowdepth/")


#Get the skeleton of a csv for data recording --------------------------------------------
#remove dups including b11 bc just care about date
sd3r[, dup.b11 := duplicated(date), by=location] 
nodup.b11<-sd3r[dup.b11 == FALSE] 

fcsv<-nodup.b11[, .(location, date_detected)]
fcsv[, snow_depth_cm := integer() ]

# write.csv(fcsv, "data/snowcam_datasheet.csv")
