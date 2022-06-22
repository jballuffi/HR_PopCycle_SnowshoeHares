# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

#use only sample periods that are 10 days or more
gps <- gps[burstlength >= 10]

gps <- gps[!is.na(burst)]

#remove outlier fixes based on distribution of easting and northings
#eastings
gps <- gps[x_proj > 300000 & x_proj < 342000] #removed 10
#northings
gps <- gps[y_proj > 6750000 & y_proj < 6770000] #removed 2 more
#check why one mean y_lat became 0 when just y_lat was non-zero???


#MCP size at 90% and 50%, keep id, winter, season, and grid
#save as RDS

#MCP at 90%
area90 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 90), by = .(id, winter, season, burst)]
setnames(area90, "a", "HRninety") #change column name

#MCP at 50%
area50 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 50), by = .(id, winter, season, burst)]
setnames(area50, "a", "HRfifty") #change column name

#merge areas of 90% and 50% volume together
areas <- merge(area90, area50, by = c("id", "winter", "season", "burst"))


##### look into outlier
areas <- areas[HRninety < 90]



#save HR areas as an RDS file in the output folder
saveRDS(areas, "output/results/hrareas.rds")


#looking into HRs > 15 ha - Liam
stancheck<-areas[HRninety >= 15]
unique(stancheck$id)

b23698<-gps[id == "23698" ]
b23698<-b23698[x_proj > 300000] # remove one big outlier
b25618<-gps[id == "25618"] 
b26316<-gps[ id == "26316"]
b26342<-gps[id == "26342"]

#new id appears after removing xproj<300000?
b26383<-gps[id == "26383"] 
ggplot(b26383, aes(x=x_proj, y=y_proj)) +
  geom_point(aes(size=0.1, colour= datetime)) #still other smaller outliers - hopefully would get caught by out-and-back cleaning later


ggplot(b23698, aes(x=x_proj, y=y_proj)) +
     geom_point(aes(size=0.1, colour= datetime))#still other outliers - hopefully would get caught by out-and-back cleaning later
ggplot(b25618, aes(x=x_proj, y=y_proj)) +
      geom_point(aes(size=0.1, colour= datetime)) 
ggplot(b26316, aes(x=x_proj, y=y_proj)) +
      geom_point(aes(size=0.1, colour= datetime)) 
ggplot(b26342, aes(x=x_proj, y=y_proj)) +
      geom_point(aes(size=0.1, colour= datetime)) 

#looking into 2020-21 HRs- Liam
w20.21<-gps[winter == "2020-2021"]

ggplot(w20.21, aes(x=x_proj, y=y_proj)) +
  geom_point(aes(size=0.1, colour= datetime, shape= as.factor(id))) 


#testing if can do plot by all individs to inspect
gps<-as.data.frame(gps)

p<-ggplot(gps, aes(x=x_proj, y=y_proj)) +
  geom_point(aes(size=0.1, colour= datetime))

plots = gps %>%
  group_by(id) %>%
  do(plots = p %+% . + facet_wrap(~deploy_id))
plots$plots[[6]]

pdf("output/figures/individbunny_GPSbydate_LGH.pdf",
    width = 10, height = 7)
plots$plots
dev.off()
