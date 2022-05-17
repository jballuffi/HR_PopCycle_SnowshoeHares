


# Prep work ---------------------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#state the UTM zone (zone 7)
utm7N <- '+proj=utm +zone=7 ellps=WGS84'

#create file path to cleaned gpd data
files <- dir("data/cleaned_gps", full.names = TRUE)



# collect files and make right -----------------------------------------------------------


#lapply a little function to fread the list of files
ls.files<-lapply(files, FUN=function(m){
  dt<-fread(m)
  return(dt)
})

#rbind the list of files
gps<-rbindlist(ls.files, fill = TRUE, use.names = TRUE)

#make ID a factor
gps[, ID := as.factor(ID)]

#create a date column
gps[, date := tstrsplit(datetime, " ", keep = 1)][, date := mdy(date)]




# Function to determine asymptote -----------------------------------------


onebun <- gps[ID == 22130]


onebun <- onebun[order(datetime)]
onebun[, diffday := date - min(date)]


onebun[diffday < 8]
onebun[diff < 15]
onebun[]

onebunsp <- SpatialPointsDataFrame(onebun[, .(x.utm, y.utm)],
                       data = onebun[, .(ID, grid, sex)],
                       proj4string = CRS(utm7N))
#why do we get a warning when we add other columns (grid, sex, etc), but 
#it doesn't work when we only use ID?

hrsize <- as.character(mcp.area(onebunsp, percent = 95, plotit = FALSE))

