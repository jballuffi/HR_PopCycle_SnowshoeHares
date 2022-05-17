


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


onebun <- gps[ID == 22130 & year == "2015-2016"]


#convert to space things and calculate MCP

testfun <- function(x, y, col1, col2, col3, utmzone, df){

  spdf <- SpatialPointsDataFrame(df[, .(x, y)],
                         data = df[, .(col1, col2, col3)],
                         proj4string = CRS(utmzone))
  
  size <- as.character(mcp.area(spdf, percent = 95, plotit = FALSE))
  
  return(size)
}


testfun(x= t1$x.utm, y = t1$y.utm, 
        col1 = t1$ID, col2 = t1$sex, col3 = t1$grid, 
        utmzone = utm7N, 
        df = t1)


#onebun <- onebun[order(datetime)]
onebun[, diffday := date - min(date)]


t1 <- onebun[diffday < 8]
t2 <- onebun[diffday < 15]
t3 <- onebun[diffday < 22]

sp1 <- SpatialPointsDataFrame(t1[, .(x.utm, y.utm)],
                       data = t1[, .(ID, grid, sex)],
                       proj4string = CRS(utm7N))

sp2 <- SpatialPointsDataFrame(t2[, .(x.utm, y.utm)],
                              data = t2[, .(ID, grid, sex)],
                              proj4string = CRS(utm7N))


sp3 <- SpatialPointsDataFrame(t3[, .(x.utm, y.utm)],
                              data = t3[, .(ID, grid, sex)],
                              proj4string = CRS(utm7N))


mcp1 <- as.character(mcp.area(sp1, percent = 95, plotit = FALSE))
mcp2 <- as.character(mcp.area(sp2, percent = 95, plotit = FALSE))
mcp3 <- as.character(mcp.area(sp3, percent = 95, plotit = FALSE))


#why do we get a warning when we add other columns (grid, sex, etc), but 
#it doesn't work when we only use ID?

hrsize <- as.character(mcp.area(onebunsp, percent = 95, plotit = FALSE))

