


# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")


# Collect home range size asymptote data -----------------------------------------

#grab 35 bunnies randomly
randbuns <- gps[id %in% sample(unique(gps$id), 35, replace = FALSE)] 

#check sample sizes for each individual-year category
randbuns[, .N, by = .(id, winter, season)]

#run the area_asym function on the sample of hares by id and by winter
asym_data <- randbuns[, area_asym(DT = .SD), by = c("id", "winter", "season")]

#remove rows where the number of collaring days didn't reach the weekly sampling interval
asym_data <- asym_data[!maxdiffday < daycount]

#create an id-winter column
asym_data[, id_winter := paste0(id, " ", winter)]



# Look at data ------------------------------------------------------------

#calculate mean home range
asym_data[, mean(area), by = id]

#take means and sd for each day count, summarising results from all individuals
asym_means <- asym_data[, .(mean(area), sd(area)), by = daycount]
names(asym_means) <- c("daycount", "mean", "sd")



ggplot(asym_means)+
  geom_line(aes(x = daycount, y = mean))
  #geom_ribbon(aes(x = daycount, ymax = mean + sd, ymin = mean - sd), alpha = .5)

ggplot(asym_data)+
  geom_line(aes(x = daycount, y = area, group = id_winter))
