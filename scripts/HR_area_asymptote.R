


# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
fread("Data/all_gps.rds")


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




  # bunintervals <- rbindlist(hrs, fill = TRUE,  use.names = TRUE)
  
  # list(
  #   area = hrs,
  #   id = unique(subdt$ID)
  # )
  # # names(bunintervals) <-"area"
  # bunintervals[, ID := unique(subdt$ID)]
  # bunintervals[, winter := unique(subdt$winter)]
  # bunintervals[, effortday := effort]
  # 
  # return(bunintervals)
  


outonebun <- area_asym(DT = onebun)


outonebun[, ID := unique(onebun$ID)]


output <- lapply(unique(randGPS$ID), function(x) {
  asymptote(randGPS[ID == x])
})
output <- list(randGPS[, asymptote(subdt = .SD), by = c("ID", "winter")])
hreffort <- rbindlist(output, fill = TRUE, use.names = TRUE)




#why do we get a warning when we add other columns (grid, sex, etc), but 
#it doesn't work when we only use ID?

hrsize <- as.character(mcp.area(onebunsp, percent = 95, plotit = FALSE))

