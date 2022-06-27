# Prep work ---------------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")



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

#MCP at 75%
area75 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 75), by = .(id, winter, season, burst)]
setnames(area75, "a", "HR75") #change column name

#MCP at 50%
area50 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 50), by = .(id, winter, season, burst)]
setnames(area50, "a", "HRfifty") #change column name

#merge areas of 90% , 75% volume together
areas.temp <- merge(area90, area75, by = c("id", "winter", "season", "burst"))
#merge the 50% volume as well
areas<- merge(areas.temp, area50, by = c("id", "winter", "season", "burst"))


##### look into outlier
areas <- areas[HRninety < 90]


#save HR areas as an RDS file in the output folder
saveRDS(areas, "output/results/hrareas.rds")








#looking into HRs > 15 ha - Liam
stancheck<-areas[HRninety >= 15]
unique(stancheck$id)

b23698<-gps[id == "23698" ]
b25618<-gps[id == "25618"] 
b26316<-gps[ id == "26316"]
b26342<-gps[id == "26342"]

#new id appears after removing xproj<300000?
b26383<-gps[id == "26383"] 
ggplot(b26383, aes(x=x_proj, y=y_proj)) +
  geom_point(aes(size=0.1, colour= datetime)) #still other smaller outliers - hopefully would get caught by out-and-back cleaning later


ggplot(b23698, aes(x=x_proj, y=y_proj)) +
     geom_point(aes(size=0.1, colour= datetime))#still other outliers - hopefully would get caught by out-and-back cleaning later

hr19.20<-ggplot(b25618, aes(x=x_proj, y=y_proj)) +
      geom_point(aes(size=0.1, colour= datetime)) + ggtitle("2019/20")  #2019/20
hr20.21<-ggplot(b26316, aes(x=x_proj, y=y_proj)) +
      geom_point(aes(size=0.1, colour= datetime)) + ggtitle("2020/21") #2020/21
hr21.22<-ggplot(b26342, aes(x=x_proj, y=y_proj)) +
      geom_point(aes(size=0.1, colour= datetime)) + ggtitle("2021/22")#2021/22

ggsave("output/figures/hr19.20_90MCP.jpeg", hr19.20, width = 8, height = 6, units = "in")
ggsave("output/figures/hr20.21_90MCP.jpeg", hr20.21, width = 8, height = 6, units = "in")
ggsave("output/figures/hr21.22_90MCP.jpeg", hr21.22, width = 8, height = 6, units = "in")

#looking into 2020-21 HRs- Liam
w20.21<-gps[winter == "2020-2021"]
unique(w20.21$id) #26316 26343 26344 26370 25226



ggplot(w20.21, aes(x=x_proj, y=y_proj)) +
  geom_point(aes(size=0.1, colour= datetime, shape= as.factor(id))) 


#testing if can do plot by all individs to inspect
gps<-as.data.frame(gps)

p<-ggplot(gps, aes(x=x_proj, y=y_proj)) +
  geom_point(aes(size=0.1, colour= datetime))


plots = gps %>%
  group_by(id) %>%
  do(plots = p %+% . + facet_wrap(~deploy_id))#check misalignments between deploy_id date and actual date
plots$plots[[6]]

pdf("output/figures/individbunny_GPSbydate_LGH-JUn23-2022.pdf",
    width = 10, height = 7)
plots$plots
dev.off()

#check if can make mcp's
# Only include three columns (id, x, and y coordinates) for making MCP's
gps.sp <- gps[, c("id", "x_proj", "y_proj")] 
coordinates(gps.sp) <- c("x_proj", "y_proj")
proj4string(gps.sp) <- CRS(utm7N)

gpsmcp <- mcp(gps.sp, percent = 90)
gpsmcp

plot(gps.sp, col = as.factor(gps.sp@data$id), pch = 16)
plot(gpsmcp, col = alpha(1:5, 0.5), add = TRUE)

#look at HR size changes based on MCP %
hr<-mcp.area(gps.sp, percent = seq(50, 100, by = 5), plotit = FALSE)
hr$perc<- seq(50, 100, by = 5)
hr<-as.data.table(hr)
hr.melt<-melt(hr, id.vars = c("perc"))

ggplot(hr.melt)+
  geom_line(aes(x = perc, y = value, group = variable))+
  labs(y = "HR Size", x = "MCP Percentage") + ylim(0, 25)
  theme_densities


df <- fortify(gpsmcp)
df$perc <- "90"


g <- ggplot(df, aes(x = long, y = lat)) +
  geom_polygon(alpha = 0, colour=c("red")) 

plots = df %>%
  group_by(id) %>%
  do(plots = g %+% . + facet_wrap(~id))#check misalignments between deploy_id date and actual date
plots$plots[[5]]


g
g + facet_wrap(~id)
