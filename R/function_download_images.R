#Function to download images from wildtrax
# 'dat' argument is the 'image_report' excel sheet from the data download on Wildtrax
# 'file.path' argument is the main folder where you want everything to be saved  (e.g., "Data/test_data/test_func/")
  #separate folders will be automatically made for each camera location


#IMPORTANT NOTE: You need to have the package "data.table" for this function to work. Code for this below.
# install.packages("data.table")
# library(data.table)


# Author: Liam Horne
# Last Updated: April 4, 2023

download.images<- function(dat, file.path) {
  
  #get list of locs
  loc <- dat[, unique(location)]
  
  for(l in loc) {
    #get  data for specific location
    specloc <- dat[location == l]
    
    #check if dir exists
    already.directory<-dir.exists(paste0(file.path, l))
    
    if (already.directory) { #then just add folders to the directory already named as the loc
      
      #get image urls from this lcoation
      url<-specloc[, `image_url(admin only)`]
      
      for(u in url) {
        #get the date from the row with the'u-th' URL
        d<-specloc[`image_url(admin only)` == u, date]  #just date, not .(date) because just want the value output
        #download image from url
        download.file(u, destfile = paste0(file.path, l, "/image_", d, ".jpg"), mode = 'wb')
      }
    } else { #if the directory doesnt exist yet
      
      #create folder in directory for this location
      dir.create(paste0(file.path, l))
      
      #get image urls from this lcoation
      url<-specloc[, `image_url(admin only)`]
      
      for(u in url) {
        #get the date from the row with the'u-th' URL
        d<-specloc[`image_url(admin only)` == u, date]  #just date, not .(date) because just want the value output
        #download image from url
        download.file(u, destfile = paste0(file.path, l, "/image_", d, ".jpg"), mode = 'wb')
      }
      
    }
    
  }
  
}
