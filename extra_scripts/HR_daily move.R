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

#merge by deploy_id to get all dates and week for that ID 
setnames(gps, c("x", "y", "t"), c("x_proj", "y_proj", "datetime")) 
# merge(gps, bsl, by="deploy_id", all.x=T)

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

did<- ref[, unique(deploy_id)]
d="24653_2018-12-17"
for (d in did) {
    r1<-ref[deploy_id==d]
    
  bsl[deploy_id == d, mindate:= r1$mindate]
}

a<-merge(ref, bsl, by="deploy_id", allow.cartesian=T, all=T)
t<-a[, .N, deploy_id]
t1<-bsl[, .N, deploy_id]
#test if fixes per deployID are same after merge
m1<-merge(t, t1, by="deploy_id", all=T) #they arent???? but does 'a' always have mroe
m1[, diff:=N.x-N.y] #yes merged one is always more
#this is because one deployid has multiple weeks, so try to get week
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
nw[, sum(V1)] #586 hare weeks

#we have 7 days, but want arbitrarily at least 85% of possible steps
wbsl[, n.steps:=.N, .(deploy_id, week)] # n steps
wbsl[, prop.steps := n.steps/((48*7)-1)]  #max fixes are 48 per day x 7 days, but minus 1 for n steps
wbsl[, hist(prop.steps)]
wbsl[sl_>22500] #nothing unrealistic 45kph=45000mph=22500m/30mins

#lets calc mean movement  per day before we clean based on prop.steps

mpd<-wbsl[, sum(sl_), .(idate, deploy_id, week, prop.steps, n.steps)]
setnames(mpd, "V1", "meter_day")
#avg meter per day per week
ampd<-mpd[, median(meter_day), .(deploy_id, week, prop.steps, n.steps)]
setnames(ampd, "V1", "avg_meter_day")
#overall avg
ampd[, median(avg_meter_day)] 

ggplot(ampd)+
  geom_point(aes(x=prop.steps, y=avg_meter_day)) +
  coord_cartesian(ylim=c(0,9000))

#lets just arbiitrailry remove below .5 success for now (could do sensitivty analyssi later)
cl.am<-ampd[prop.steps>=0]
cl.am[, hist(avg_meter_day)]


#merge with HR data
sub.dat<-dat[, .(deploy_id, week, M90, M75, M50, K90, K75, K50)]

hrm<-merge(cl.am, sub.dat, by=c("deploy_id", "week"), all.x=T)
#remove buns with no HRs
hrm<-hrm[!is.na(M90)]

#plot areas against fix rate
ggplot(hrm) +
  geom_jitter(aes(x= avg_meter_day, y = M90), colour="red", width = 0.5) +
  geom_jitter(aes(x= avg_meter_day, y = M75), colour="green", width = 0.5) +
  geom_jitter(aes(x= avg_meter_day, y = M50), colour="blue", width = 0.5) +
  coord_cartesian(xlim=c(0,2500), ylim=c(0,20)) # zoom in on bulk of data






