


# Prep work ---------------------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#state the UTM zone (zone 7)
utm7N <- '+proj=utm +zone=7 ellps=WGS84'

#create file path to cleaned gps data
files <- dir("data/Cleaned_gps", full.names = TRUE)



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


onebun <- gps[ID == "22130" & year == "2015-2016"]

onebun[, diffday := date - min(date)]

mcp_area(x= 'x.utm', y ='y.utm', 
        extra_cols = c('ID', 'sex', 'grid'), 
        utmzone = utm7N, 
        dt = t1)

buns <- list(
t8 = onebun[diffday < 8],
t14 = onebun[diffday < 15],
t21 = onebun[diffday < 22],
t28 = onebun[diffday < 29],
t35 = onebun[diffday < 36])



lapply(c(8, 15, 22), function(APPLES) {
  mcp_area(onebun[diffday < APPLES], x = 'x.utm', 'y.utm', 'year', utm7N)
})

bunintervals <- rbindlist(buns, fill = TRUE,  use.names = TRUE)



test <- function(bunyear){
  
  #calculate the difference between a gps fix date and the first date of the bunny year
  bunyear[, diffday := date - min(date)]
  

  
  
}







#why do we get a warning when we add other columns (grid, sex, etc), but 
#it doesn't work when we only use ID?

hrsize <- as.character(mcp.area(onebunsp, percent = 95, plotit = FALSE))

