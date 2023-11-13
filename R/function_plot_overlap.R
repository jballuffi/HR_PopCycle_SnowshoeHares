#function that calculates mcp from a data.table or data.frame
#Arguments: x = x coord; y = y coord; col1-col3 = additional data to include
# utmzone = the utm zone; gpsdata = the data.table

plot_mcp <- function(dt){
  
  #create spatial data
  sampleSP <- SpatialPointsDataFrame(dt[, .SD, .SDcols = c("x_proj", "y_proj")],
                                     data = dt,
                                     proj4string = CRS(utm7N))
  
  #calculate the mcp
  sampleMCP <- mcp(sampleSP[,1], percent = 90)
  
  #pull the eartag name from week id
  idname <- dt[1, as.character(tstrsplit(id_week, "_", keep = 1))]
  
  #plot the mcps by the week id
  out <- st_as_sf(sampleMCP) %>% ggplot(., aes(fill = id)) + geom_sf(alpha = 0.5) +
    scale_fill_discrete(name = "Week ID")
  
  #save the figure with the eartag name
  ggsave(filename = paste("output/homeranges/",idname,".jpeg"), out, width = 6, height = 3, unit = "in")
  
  
}
