
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]

#round winter day 
gps[, weekwinterday := round(weekwinterday)]

#items we will measure home ranges by
weeksplit <- c("id", "winter", "deploy_id", "week", "weekdate", "weekwinterday", "grid")




# grab 15 random individuals and run the mcp function  --------------------

#create a unique week ID
gps[, id_week := paste0(id, "_", weekdate)]

#randomly select 10 individuals from GPS data
sample <- gps[id %in% sample(gps$id, 15, replace = FALSE), 
              .(id, id_week, x_proj, y_proj)]


sample[, plot_mcp(dt = .SD), id]

