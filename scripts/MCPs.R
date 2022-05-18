


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

#get rid of year column
gps[, year := NULL]

#create a date column
gps[, date := tstrsplit(datetime, " ", keep = 1)][, date := mdy(date)]

#categorize gps fixes into winters
gps[date > "2015-10-01" & date < "2016-04-01", winter := "2015-2016"]
gps[date > "2016-10-01" & date < "2017-04-01", winter := "2016-2017"]
gps[date > "2017-10-01" & date < "2018-04-01", winter := "2017-2018"]
gps[date > "2018-10-01" & date < "2019-04-01", winter := "2018-2019"]

#remove anything that doesn't fall into winter
gps <- gps[!is.na(winter)]





# Function to determine asymptote -----------------------------------------

#grab just one bunny year
onebun <- gps[ID == "22130" & winter == "2016-2017"]
#calculate difference between fix date and first date
onebun[, diffday := date - min(date)]

#grab 30 bunnies randomly
randIDs <- sample(unique(gps$ID), 30, replace = FALSE)
randGPS <- gps[ID %in% randIDs] 
#calculate difference between fix date and first date by ID and by winter (year)
randGPS[, diffday := date - min(date), by = .(ID, winter)]



asymptote <- function(subdt){
  
  effort <- c(8, 15, 22, 29, 36)
  
  hrs <- lapply(effort, function(n) {
    mcp_area(subdt[diffday < n], x = 'x.utm', 'y.utm', 'winter', utm7N)
  })
  
  # bunintervals <- rbindlist(hrs, fill = TRUE,  use.names = TRUE)
  
  list(
    area = hrs,
    id = unique(subdt$ID)
  )
  # names(bunintervals) <-"area"
  # bunintervals[, ID := unique(subdt$ID)]
  # bunintervals[, winter := unique(subdt$winter)]
  # bunintervals[, effortday := effort]
  # 
  # return(bunintervals)
  
}

outonebun <- asymptote(subdt = onebun)

output <- lapply(unique(randGPS$ID), function(x) {
  asymptote(randGPS[ID == x])
})
output <- list(randGPS[, asymptote(subdt = .SD), by = c("ID", "winter")])
hreffort <- rbindlist(output, fill = TRUE, use.names = TRUE)




#why do we get a warning when we add other columns (grid, sex, etc), but 
#it doesn't work when we only use ID?

hrsize <- as.character(mcp.area(onebunsp, percent = 95, plotit = FALSE))

