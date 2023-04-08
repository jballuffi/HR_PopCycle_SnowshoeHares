#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in cam snow data
sd<-fread("Data/liam_test_data/BOUTIN_Kluane_image_report.csv")
#only keep timelapse photos
sd<-sd[trigger == "Time Lapse"]
#remove before winter 2019-20
sd<-sd[date_detected >= "2019-10-01 00:00:00"]
#get the julian day
sd[, jday := lubridate::yday(date_detected)]
#get the year
sd[, yr := lubridate::year(date_detected)]

#get oct 1-April 30 (<=121 >=275) ( leap year)
#get oct 1-April 30 (<=120 >=274) (non leap year)
sd1<-sd[(yr == 2020 & jday <= 121) | (yr != 2020 & jday <= 120)]
sd2<-sd[(yr == 2020 & jday >= 275) | (yr != 2020 & jday >= 274)]
sd3<-rbind(sd1, sd2)

#get a col just date and just time
sd3[, date := tstrsplit(date_detected, " ", keep=1)]
sd3[, time := tstrsplit(date_detected, " ", keep=2)]

#get unique locations and years for timelapse data
un<-sd3[, unique(location), by=yr]
#check whether only 1 image per date
a<-sd3[, .N, .(date, location)]
b<-a[N >1, unique(date), location]
b[, .N, location] #only 4 locations from desired cams - CH and SU were taking pics at 1100 and 1200

#there are pics from 2 locations at start of B11_1 (pre oct 18 2019)
b11<-sd3[location=="B11_1" & yr <= 2019 & jday < 291] #17 *2 pics =34

#remove duplicates (only want one depth per day (ideally at 1100)
#BUT DONT remove dups for B11_1 bc it's weird and has to be dealt with separate
sd3[location != "B11_1", dup := duplicated(date), location] 
sd3<-sd3[dup == FALSE | is.na(dup)]

locs<-c("CH", "C10_6", "B11_1", "A10_4", "A9_3", "B9", "SU", "B8_1", "A7_5")
sd3r<- sd3[location %in% locs]

# test<-sd3r[c(1, 236:239, 340:342)]
# dat<-sd3r[c(1, 236:239, 340:342)]
# l="B9"
# loc.col<-"location"
# url.col = "image_url(admin only)"
# image.id.col<-"date"
#get identifier column for each image (e.g., date or datetime)
setnames(sd3r, "location", "location")
setnames(sd3r, "image_url(admin only)", "image_url")
setnames(sd3r, "date", "image_id")


download.images<- function(dat, file.path) {
  
  #get list of locs
  loc <- dat[, unique(location)]
  
  for(l in loc) {
    #get  data for specific location
    specloc <- dat[location == l]

    #check if dir exists
    already.directory<-dir.exists(paste0(file.path, l))
    
    if (already.directory) { #then just add folders to the directory already named as the loc
      
      #get image urls from this location
      url<-specloc[, image_url]
      
      for(u in url) {
        
        #get the image id from the row with the 'u-th' URL
        iid<-specloc[image_url == u, image_id]  #just image_id, not .(image_id) because just want the 'Values' output
        #download image from url
        download.file(u, destfile = paste0(file.path, l, "/image_", iid, ".jpg"), mode = 'wb')
      }
    } else { #if the directory doesn't exist yet
      
      #create folder in directory for this location
      dir.create(paste0(file.path, l))
      
      #get image urls from this location
      url<-specloc[, image_url]
      
      for(u in url) {
        
        #get the image id from the row with the 'u-th' URL
        iid<-specloc[image_url == u, image_id]  #just image_id, not .(image_id) because just want the 'Values' output
        #download image from url
        download.file(u, destfile = paste0(file.path, l, "/image_", iid, ".jpg"), mode = 'wb')
      }
    }
  }
}

#run the functino for each folder you want downloaded
# download.images(dat=sd3r, file.path = "Data/liam_test_data/snow depth camera downloads/")
download.images(dat=test, file.path = "Data/liam_test_data/test_func/")
#run the function again but just with b11_1 to get all the photos, and will then just ignore the 17 wrong ones
"%notin%" <- Negate("%in%")
b11<-sd3r[location=="B11_1" & yr <= 2019 & jday < 291]
b11[, addon := rep(1:2, length.out=.N)]
b11[, image_id := paste0(image_id, "_", addon)]
sb11<-sd3r[location=="B11_1"]
sb11<- sb11[date_detected %notin% b11$date_detected]
di.b11 <-rbind(sb11, b11, fill=T)

download.images(dat=di.b11, file.path = "Data/liam_test_data/snow depth camera downloads/")




sd3r[, uniqueN(yr), location]
sd3r[, uniqueN(date), location]

#get the skeleton of a csv for data recording
fe<-sd3r[, .(location, date_detected)]
fe[, snow_depth_cm := NA]

write.csv(fe, "data/snowdepth_datasheet.csv")
