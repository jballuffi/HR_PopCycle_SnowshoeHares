#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)
library(amt)
library(dplyr)

dat <- readRDS("output/results/compileddata.rds")

gps <- readRDS("Data/all_gps.rds")
#rename cols to match AMT syntax
setnames(gps, c("x_proj", "y_proj", "datetime"), c("x", "y", "t")) 
#workaround to get final data is make id include the week bc cant get it to work 


a<-gps[, unique(fixrate), deploy_id]
a[, .N, V1] # 30 min FR most common, and others can rarefy to 30 mins

#convert to data frame b/c AMT doesnt like DT
gpsdf <- as.data.frame(gps)

#Resample to 30min fix rate and calculate SL -----------------------------------------------

#nest the data based on deploy id (bc that is a collar)
bunnest <- gpsdf %>% group_by(deploy_id) %>% nest()

#Make a track for each nested deploy id
trk.nest <- bunnest %>%
  mutate(data = lapply(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS(utm7N)) 
  }))

#summarize the fix rate for each deploy id
fr <- trk.nest %>% dplyr::mutate(fr = lapply(data, summarize_sampling_rate, time_unit="min")) %>% 
  dplyr::select(deploy_id, fr) %>% unnest(cols = c(fr))

#try with one animal
y <- trk.nest$data[[25]]
#calculate SL and TAs "steps_by_burst" which removes burst numbers with only 1 fix 
#bc cant calc SL with 1 fix
y1<-  y%>% track_resample(rate = minutes(30), tolerance = minutes(5)) %>%
  steps_by_burst()

#now apply resampling and step by bursts as a function to all the animals
trk.nest.rs30 <- trk.nest %>%
  mutate(steps = lapply(data, function(x) {
    x %>%
      amt::track_resample(rate = minutes(30), tolerance = seconds(300)) %>%
      amt::steps_by_burst() 
  }))
trk.nest.rs30

#unnest the buns and steps and save as a data table
buns.unnest30 <- trk.nest.rs30 %>% dplyr::select(deploy_id, steps) %>% unnest(cols = c(steps))
buns.unnest30 <- as.data.table(buns.unnest30) #86042 rows
buns.unnest30[is.na(burst_)] #no NAs for burst_

# buns.unnest30 <- trk.nest.rs30 %>% dplyr::select(deploy_id, steps, data) %>% unnest(cols = c(steps))
# nam<-c("deploy_id","burst_","x1_","x2_","y1_","y2_","sl_","direction_p","ta_","t1_","t2_" ,"dt_")
# a <- buns.unnest30 %>% dplyr::select(nam, data) %>% unnest(cols = c(data))
# a <- as.data.table(a) 
# a[, unique(week), ]


bsl<-copy(buns.unnest30)


#SHOULD START NEW SCRIPT-----------------------------------------

# saveRDS(bsl, "data/liam_test_data/SL_gps_30min_LGH.rds")
lapply(dir('R', '*.R', full.names = TRUE), source)
bsl<-readRDS("data/liam_test_data/SL_gps_30min_LGH.rds")
dat <- readRDS("output/results/compileddata.rds")
gps <- readRDS("Data/all_gps.rds")


#get date range for eacvh week and deployid in gps
gps[, mindate := min(idate), by=.(week, deploy_id)]
gps[, maxdate := max(idate), by=.(week, deploy_id)]
# get unique min max fo week and depid
ref<-gps[, .(unique(mindate), unique(maxdate)), by=.(week, deploy_id)]
setnames(ref, c("V1", "V2"), c("mindate", "maxdate"))

#if t1_ is in that date range, then give it the appropriate week
bsl[, date:= as.character((t1_))]
bsl[, date:=tstrsplit(date, " ", keep = 1)]
bsl[, date:=lubridate::ymd(date)]
bsl[, idate:=as.IDate(date)]

a<-merge(ref, bsl, by="deploy_id", allow.cartesian=T, all=T)
#a always has more  because one deployid has multiple weeks, so try to get week
a[idate >= mindate & idate <= maxdate, week.merge := week]
wbsl<-a[!is.na(week.merge)]
#testagain
t<-wbsl[, .N, deploy_id]
t1<-bsl[, .N, deploy_id]
m1<-merge(t, t1, by="deploy_id", all=T) #they arent???? but does 'a' always have mroe
m1[, diff:=N.x-N.y] #FIXED!!



#at 30 min fix rate,how many fixes per week?----------
#calculate how many days are in each week using number of unique dates
wbsl[, weeklength := length(unique(idate)), by = .(deploy_id, week)]

#take only weeks that have over 6 days of sampling
wbsl <- wbsl[weeklength > 6] #83135

#get number of full weeks per deployid
wbsl[, n.hare.weeks := uniqueN(week), by = deploy_id]

#how many full weeks total
nw <- wbsl[, unique(n.hare.weeks), by = deploy_id]
nw[, sum(V1)] #586 hare weeks (down from 654  bc some weeks dont have 7 days now bc of resample)

wbsl[, weekdate := mean(unique(idate)), by = .(deploy_id, week)]

#check that ha weeks in sl data and gps data are the same
# t<-wbsl[, unique(weekdate), deploy_id]
# t1<-gps[, unique(weekdate), deploy_id]
# m1<-merge(t, t1, by=c("deploy_id","V1"), all.x=T)
# wbsl[deploy_id=="23717_2017-12-13", unique(weekdate)]
# gps[deploy_id=="23717_2017-12-13", unique(weekdate)] 
# gps[deploy_id=="23717_2017-12-13", unique(idate), week] 
#this is different bc Jules did the mean on actual data, so not always the actual center point of week


#we have 7 days, but want a certain % of possible steps
wbsl[, n.steps:=.N, .(deploy_id, week)] # n steps
wbsl[, prop.steps := n.steps/((48*7)-1)]  #max fixes are 48 per day x 7 days, but minus 1 for n steps
wbsl[, hist(prop.steps)]
wbsl[sl_>22500] #nothing unrealistic 45kph=45000mph=22500m/30mins

#lets calc mean movement  per day before we clean based on prop.steps

mpd<-wbsl[, sum(sl_), .(idate, deploy_id, week, weekdate, prop.steps, n.steps)]
setnames(mpd, "V1", "meter_day")
#avg meter per day per week
ampd<-mpd[, median(meter_day), .(deploy_id, week, weekdate, prop.steps, n.steps)]
setnames(ampd, "V1", "avg_meter_day")
#overall avg
ampd[, median(avg_meter_day)] 


ggplot(ampd)+
  geom_point(aes(x=prop.steps, y=avg_meter_day)) +
   coord_cartesian(ylim=c(0,9000))

#lets just arbiitrailry remove below .5 success for now (could do sensitivty analyssi later)
cl.am<-ampd[prop.steps>=0.5] #REMEMBER THAT THIS %FIXES DOESNT REPRESETN OUR ACTUAL DATA
#WE CANT USE SINGLE FIXES BC NO STEP, AND CANT COMPARE DISTANCES OVER VARIABLE TIMES aSSUMING STRAIGHT LINE MOVEMENT
cl.am[, hist(avg_meter_day)]


#merge with HR data
sub.dat<-dat[, .(deploy_id, week, M90, M75, M50, K90, K75, K50)]

hrm<-merge(cl.am, sub.dat, by=c("deploy_id", "week"), all.x=T)
#remove buns with no HRs
hrm<-hrm[!is.na(M90)]

#melt for better plotting
mhr<-melt(hrm, na.rm = FALSE, measure.vars = c("M90", "M75", "M50"),
          variable.name = "MCP_percentile", value.name="MCP_area")


#plot areas against fix rate
# ggplot(hrm) +
#   geom_jitter(aes(x= avg_meter_day, y = M90), colour="red", width = 0.5) +
#   geom_smooth(aes(x= avg_meter_day, y = M90), colour="red", method='lm', formula= y~x) +
#   geom_jitter(aes(x= avg_meter_day, y = M75), colour="green", width = 0.5) +
#   geom_smooth(aes(x= avg_meter_day, y = M75), colour="green", method='lm', formula= y~x) +
#   geom_jitter(aes(x= avg_meter_day, y = M50), colour="blue", width = 0.5) +
#   geom_smooth(aes(x= avg_meter_day, y = M50), colour="blue", method='lm', formula= y~x) 

noz<-ggplot(mhr, aes(x=avg_meter_day, y=MCP_area, colour=MCP_percentile)) +
    geom_jitter() +
    geom_smooth(method='lm', formula= y~x) +
  labs(x="Weekly Avg Movement Rate (meters/day)", y="MCP Area (Ha)", 
       title="30 min fixes, minimum of 50% fix success per week")

# submhr<-  mhr[avg_meter_day<10000]
# submhr[MCP_area>20] #The HR outlier is less likely due to daily movements and more likely shifted areas


wz<-ggplot(mhr, aes(x=avg_meter_day, y=MCP_area, colour=MCP_percentile)) +
    geom_jitter() +
    geom_smooth(method='lm', formula= y~x) +
  labs(x="Weekly Avg Movement Rate (meters/day)", y="MCP Area (Ha)",
       title="Same as left figure, but zoomed in on bulk of data") + 
  coord_cartesian(xlim=c(0,2000), ylim=c(0,15)) # zoom in on bulk of data

(HRmoveplots <- ggarrange(noz, wz, ncol = 2, nrow = 1))


ggsave("output/figures/HR_dailymovement.jpeg", HRmoveplots, width = 17, height = 9, units = "in")


ggplot(mhr, aes(x=prop.steps, y=MCP_area, colour=MCP_percentile)) +
  geom_point() +
  # geom_smooth(method='lm', formula= y~x) +
  labs(x="Fix Success", y="MCP Area (Ha)", 
       title="Fix Success after 30 min resample")
  
#START NEW SCRIPT---------------------------------------------- 
  
#movement Outliers seems to be an individual that did large DAILY movements back and forth betwn two areas
  
  #look into ouitliers by redoing plots by id 
  ##UTM PROJECTION DOESNT SEEM TO BE IN RIGHT LOCATION (POINTS ARE IN ALASKA)?!?!
  cl.am[avg_meter_day>10000]
  ampd[deploy_id=="23717_2017-12-13"]
  

  gpsdf<-as.data.frame(gps)
  
  p<-ggplot(gpsdf, aes(x=x_proj, y=y_proj)) +
    geom_point(aes(size=0.1, colour= idate))
  
  plots = gpsdf %>%
    group_by(deploy_id) %>%
    do(plots = p %+% . + facet_wrap(~deploy_id + weekdate))
  plots$plots[[91]]
  
  # pdf("output/figures/GPSfixes_each_weekly_HR.pdf",
  #     width = 10, height = 7)
  # plots$plots
  # dev.off()



