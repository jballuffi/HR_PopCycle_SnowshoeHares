

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("data/all_gps.rds")

#remove any data that is in a week that didn't hit 7 days
gps <- gps[weeklength > 6]

#round winter day 
gps[, weekwinterday := round(weekwinterday)]


#items we will measure home ranges by
weeksplit <- c("id", "winter", "deploy_id", "week", "weekdate", "weekwinterday", "grid", "Food")


# calculate weekly Kernel Density areas -----------------------------------
#this uses our pre-made function called "kernel_area" in the "R/" folder
#kernels at 90, 70, and 50%

kern90 <- gps[, kernel_area(gpsdata = .SD, utmzone = utm7N, vol = 90), by = weeksplit]
setnames(kern90, "V1", "K90")

kern75 <- gps[, kernel_area(gpsdata = .SD, utmzone = utm7N, vol = 75), by = weeksplit]
setnames(kern75, "V1", "K75")

kern50 <- gps[, kernel_area(gpsdata = .SD, utmzone = utm7N, vol = 50), by = weeksplit]
setnames(kern50, "V1", "K50")



# calculate weekly MCP areas ---------------------------------------------- 
#this uses our pre-made function called "mcp_area" in the "R/" folder
#MCPs at 90, 70, and 50%

mcp90 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 90), by = weeksplit]
setnames(mcp90, "a", "M90") #change column name

mcp75 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 75), by = weeksplit]
setnames(mcp75, "a", "M75") #change column name

mcp50 <- gps[, mcp_area(.SD, x = "x_proj", y = "y_proj", utmzone = utm7N, vol = 50), by = weeksplit]
setnames(mcp50, "a", "M50") #change column name



# merge MCP home range results and Kernel density results ---------------------------------------------------

#merge MCP areas of 90% , 75% volume together
mcpfull <- merge(mcp90, mcp75, by = weeksplit)
mcpfull2 <- merge(mcpfull, mcp50, by = weeksplit)

#merge kernal densities of 90%, 75%, and 50% together
kernelfull <- merge(kern90, kern75, by = weeksplit)
kernelfull2 <- merge(kern50, kernelfull, by = weeksplit)

#merge MCPs and kernal density home ranges
mcpkernel <- merge(mcpfull2, kernelfull2, by = weeksplit)



# Get fix rates for each burst and merge with results ---------------------

#obtain unique info for fix rates
gps.sub <- gps[, unique(deploy_id), by = .(id, winter, week, fixrate, n.fixes)]
setnames(gps.sub, "V1", "deploy_id") #change column name

#merge the fix rates with the HR areas
FRsplit <- c("deploy_id", "week", "winter", "id")
areas <- merge(mcpkernel, gps.sub, by = FRsplit)



# Create seasons and find last home range -----------------------------

areas[, mnth := lubridate::month(weekdate)]

#december and january are early winter
areas[mnth == 12 | mnth == 1, season := "early"]

#february and march are last winter
areas[mnth == 2 | mnth == 3, season := "late"]

#take the last home range week, make new variable by id
areas[, lastweek := max(weekdate), id]

#is it the last home range for an individual?
areas[weekdate == lastweek, lastHR := "yes"]
areas[is.na(lastHR), lastHR := "no"]



# Are last home ranges outliers? If so remove --------

#first remove impossible home ranges (> 36 ha). That's a whole grid
areas <- areas[!M90 > 36]

#calculate z score
areas[, zscore := abs((M90 - mean(M90))/sd(M90))]

#new table of just outliers
outliers <- areas[zscore > 3]
#who are the indiduals in this table? 
outlier.inds <- outliers[, unique(id)]
areas[id %in% outlier.inds]


#remove any home ranges that are statistical outliers
areas <- areas[!zscore > 3]

#remove cases where the last home range is nearly 0. suspected mortality, but not in records
areas <- areas[!(lastHR == "yes" & M90 < 0.1)]



# explore data basics -----------------------------------------------------

#plot areas against fix rate
ggplot(areas) +
  geom_jitter(aes(x= fixrate, y = K90), colour="red", width = 0.5) +
  geom_jitter(aes(x= fixrate, y = K75), colour="green", width = 0.5) +
  geom_jitter(aes(x= fixrate, y = K50), colour="blue", width = 0.5) 

#plot areas against number of fixes
ggplot(areas) +
  geom_jitter(aes(x= n.fixes, y = M90), colour="red", width = 0.5) +
  geom_jitter(aes(x= n.fixes, y = M75), colour="green", width = 0.5) +
  geom_jitter(aes(x= n.fixes, y = M50), colour="blue", width = 0.5) +
  coord_cartesian(ylim = c(0, 100))

#plot home range sizes of kernels against MCPS
ggplot(areas)+
  geom_point(aes(x = M90, y = K90), color = "red")+
  #geom_point(aes(x = M75, y = K75), color = "green")+
  #geom_point(aes(x = M50, y = K50), color = "blue")+
  xlim(0, 40)+
  ylim(0, 40)

#plot kernel 90 against kernel 50
ggplot(areas)+
  geom_point(aes(x = M50, y = M90))+
  xlim(0, 20)+
  ylim(0, 20)



# save figures ------------------------------------------------------------

#save HR areas as an RDS file in the output folder
saveRDS(areas, "data/hrareas.rds")
