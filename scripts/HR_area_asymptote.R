


# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")


# Collect home range size asymptote data -----------------------------------------

#grab 30 bunnies randomly
randbuns <- gps[ID %in% sample(unique(gps$ID), 30, replace = FALSE)] 

#check sample sizes for each individual-year category
randbuns[, .N, by = .(ID, winter)]

#run the area_asym function on the sample of hares by ID and by winter
asym_data <- randbuns[, area_asym(DT = .SD), by = c("ID", "winter")]

#remove rows where the number of collaring days didn't reach the weekly sampling interval
asym_data <- asym_data[!maxdiffday < daycount]

#create an ID-winter column
asym_data[, IDwinter := paste0(ID, " ", winter)]



# Look at data ------------------------------------------------------------

#calculate mean home range
asym_data[, mean(area), by = ID]

#take means and sd for each day count, summarising results from all individuals
asym_means <- asym_data[, .(mean(area), sd(area)), by = daycount]
names(asym_means) <- c("daycount", "mean", "sd")



ggplot(asym_means)+
  geom_line(aes(x = daycount, y = mean))

ggplot(asym_data)+
  geom_line(aes(x = daycount, y = area, group = IDwinter, color = ID))
