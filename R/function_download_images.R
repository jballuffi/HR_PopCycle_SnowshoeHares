#Function to download images from wildtrax
# 'dat' argument is the 'image_report' excel sheet from the data download on Wildtrax
# 'file.path' argument is the primary folder within your R Project where you want everything to be saved (e.g., "Data/test_data/test_func/")
      # Separate folders will be automatically made for each camera location


#IMPORTANT NOTE: You need to have the package "data.table" for this function to work. Code for this below.
# install.packages("data.table")
# library(data.table)


# Author: Liam Horne
# Last Updated: April 4, 2023

download.images<- function(dat, file.path, loc.col, url.col) {
  
  #get list of locs
  setnames(dat, loc.col, "location")
  loc <- dat[, unique(location)]
  
  for(l in loc) {
    #get  data for specific location
    specloc <- dat[location == l]
    
    #check if dir exists
    already.directory<-dir.exists(paste0(file.path, l))
    
    if (already.directory) { #then just add folders to the directory already named as the loc
      
      #get image urls from this location
      setnames(specloc, url.col, "image_url")
      url<-specloc[, image_url]
      
      for(u in url) {
        
        #get the date from the row with the'u-th' URL
        d<-specloc[image_url == u, date]  #just date, not .(date) because just want the value output
        #download image from url
        download.file(u, destfile = paste0(file.path, l, "/image_", d, ".jpg"), mode = 'wb')
      }
    } else { #if the directory doesnt exist yet
      
      #create folder in directory for this location
      dir.create(paste0(file.path, l))
      
      #get image urls from this location
      setnames(specloc, url.col, "image_url")
      url<-specloc[, image_url]
      
      for(u in url) {
        #get the date from the row with the'u-th' URL
        d<-specloc[image_url == u, date]  #just date, not .(date) because just want the value output
        #download image from url
        download.file(u, destfile = paste0(file.path, l, "/image_", d, ".jpg"), mode = 'wb')
      }
    }
  }
}

# download.images(dat=test, file.path = "Data/liam_test_data/test_func/", url.col= "image_url(admin only)")


#TEST
test<-sd3r[c(236:239)]
test[1, `image_url(admin only)` := "https://images.pexels.com/photos/1563356/pexels-photo-1563356.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
test[2, `image_url(admin only)` := "https://images.pexels.com/photos/36717/amazing-animal-beautiful-beautifull.jpg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
test[3, `image_url(admin only)` := "https://images.pexels.com/photos/1366630/pexels-photo-1366630.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
test[4, `image_url(admin only)` := "https://images.pexels.com/photos/2088210/pexels-photo-2088210.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
test[, location := c("A1", "A2", "A2", "A3")]
test1<-test[, .(location, date`image_url(admin only)`)]
names(test1)
download.images(dat=test1, file.path = "Data/liam_test_data/test_func/", loc.col = "location",
                url.col = "image_url(admin only)")

