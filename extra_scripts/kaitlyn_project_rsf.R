
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#prep gps data before converting to spatial data
gps <- gps[yr == 2017 & grid == "Kloo"]
gps[, id := as.factor(id)]
gps <- gps[, .(id, x_proj, y_proj)]

#convert gps data to spatial data
coordinates(gps) <- c("x_proj", "y_proj")
proj4string(gps) <- utm7N

#create mcps, it automatically runs by the id column in gps
#mcps output is a list of mcps essentially
mcps <- mcp(gps)
plot(mcps)
mcps

#try out the spsample function. SEE HOW IT WORKS
#this line runs the spsample function on just one individual "22130"
test <- spsample(mcps[mcps$id == 22130,], n = 500, type = "regular")
#now convert to a data frame
out <- as.data.frame(test)
#create and id column and a status column
out$id <- 22130
out$status <- "available"

#turn that into a function that will run on just an argument for ID
#note for thisI just used 500 individuals
getavail <- function(id){
  locs <- spsample(mcps[mcps$id == id,], n = 500, type = "regular")
  out <- as.data.frame(locs)
  out$id <- id
  out$status <- "available"
  return(out)
}

#test the function on one bunny
onebun <- getavail(24553)

#now create the list of individuals
ids <- as.list(mcps$id)

#lapply the custom function to the list of individuals
final <- lapply(ids, getavail)
#the output will be a list of dataframes
#rbindlist all those dataframes together if you like
final <- rbindlist(final)

#plot in ggplot just to confirm it worked
ggplot(final)+
  geom_point(aes(x = x1, y = x2, color = id))

