


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


asymptote <- function(subdt){
  
  #calculate the difference between a gps fix date and the first date of the bunny year
  subdt[, diffday := date - min(date)]
  
  effort <- c(8, 15, 22, 29, 36)
  
  hrs <- lapply(effort, function(n) {
    mcp_area(subdt[diffday < n], x = 'x.utm', 'y.utm', 'year', utm7N)
  })
  
  bunintervals <- rbindlist(hrs, fill = TRUE,  use.names = TRUE)
  
  names(bunintervals) <-"area"
  bunintervals[, ID := unique(onebun$ID)]
  bunintervals[, year := unique(onebun$year)]
  bunintervals[, effortday := effort]
  
  return(bunintervals)
  
}

output <- asymptote(subdt = onebun)


output <- list(gps[, asymptote(subdt = onebun), by = c("ID", "year")])

hreffort <- rbindlist(output, fill = TRUE, use.names = TRUE)

#why do we get a warning when we add other columns (grid, sex, etc), but 
#it doesn't work when we only use ID?

hrsize <- as.character(mcp.area(onebunsp, percent = 95, plotit = FALSE))

