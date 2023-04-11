#Function to download images from Wildtrax
# 'dat' argument is the 'image_report' excel sheet from a data download on Wildtrax (need admin priveleges to access image URLs)
# 'file.path' argument is the primary folder within your R Project where you want the images to be saved 
    # (e.g., "Data/cameras/" - make sure it ends with a "/")
    # Separate folders will be automatically made for each camera location


#IMPORTANT NOTE: You need to have the package "data.table" for this function to work.
# install.packages("data.table")
# library(data.table)


# Author: Liam Horne
# Last Updated: April 5, 2023

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

# download.images(dat=test, file.path = "Data/liam_test_data/test_func/", url.col= "image_url(admin only)")


#TEST
# test<-sd3r[c(236:239)]
# test[1, `image_url(admin only)` := "https://images.pexels.com/photos/1563356/pexels-photo-1563356.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
# test[2, `image_url(admin only)` := "https://images.pexels.com/photos/36717/amazing-animal-beautiful-beautifull.jpg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
# test[3, `image_url(admin only)` := "https://images.pexels.com/photos/1366630/pexels-photo-1366630.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
# test[4, `image_url(admin only)` := "https://images.pexels.com/photos/2088210/pexels-photo-2088210.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"]
# test[, location := c("A1", "A2", "A2", "A3")]
# setnames(test, "location", "camera_site")
# 
# #create the export I will put on GitHub as example
# test1<-test[, .(camera_site, date,`image_url(admin only)`)]
# names(test1)
# setnames(test1, "camera_site", "location") #Change the name of the column that has your camera locations/names
# setnames(test1, "image_url(admin only)", "image_url") #change the name of the column that has your image URLs
# setnames(test1, "date", "image_id") #change the name of the column that has your image id info (e.g., date), which will be put into the file name of each image
# 
# download.images(dat=test1, file.path = "Data/liam_test_data/test_func/")


