#What is date range, SCRipt 2 SAYS OCT1-APR30?????????)

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
#transect snow depth (just track transect data (missing 2017-2019 snow data))
sd1 <- fread("data/Kluane_PredTrans_SnowDepth.csv")

#data from alice (includes 2017-2019 when they are not in the track transect database, 
# and peter was colelcting extra snow measurements)
sd2 <- fread("data/snowdepthdataentry.csv")

#grid snow depths
ag <- fread("data/Snow_grid_agnes.csv")
jo <- fread("data/Snow_grid_jo.csv")
kl <- fread("data/Snow_grid_kloo.csv")




# Prep snow depth data from predator transects (but not in the main pred transect database) --------

#rename cols
names(sd2) <- c("where", "Date", "sdepth", "Comments")

#make date a date
sd2[, Date := dmy(Date)]
sd2[, unique(year(Date))]

#clear old data out
sd2 <- sd2[Date >= "2015-10-01"]

#remove comments
sd2[, Comments := NULL]

# remove faulty values (36 where sdepth = -9999)
sd2<-sd2[!sdepth < 0]


#check the years we have left
sd2[, unique(year(Date))] #ends at 2020
sd2[, unique(Date)]
enddate <- sd2[, max(Date)] #define the last date in Alice's data



#Prep other transect snow depth -------------------------------

#add  label for transect data (just the segment because that is a unique measure and
#tells that it is from pred transect data)
sd1[, where := Segment]

#convert to date
sd1[, Date := dmy(DateTrans)]

#remove data from before November 1, 2015 (the cutoff date for HR analysis)
sd1 <- sd1[Date >= "2015-10-01"]

#remove NA snow depths 
sd1[!is.na(SnowDepthStn), unique(year(Date))] #(ask Alice about no data 2017-19) -it is in the other snowdata from peter
sd1 <- sd1[!is.na(SnowDepthStn)]
sd1[, unique(year(Date))]

#remove repeat snow measure per segment (repeated because of animal tracks)
sd1 <- sd1[, unique(SnowDepthStn), by = .(Date, where)]
setnames(sd1, "V1", "sdepth")

#check values same
a<-sd1[year(Date) < 2017 & year(Date) > 2015]
b<-sd2[year(Date) < 2017 & year(Date) > 2015]
sd1[year(Date) < 2017 &rbind(sd1, sd2)
merge(sd2, sd1, by=c("Date", "sdepth"), all=T)
ggplot() + geom_point(aes(x=sd1$sdepth, y=sd2$sdepth))

#subset to only dates after SD2 ends
sd1late <- sd1[Date > enddate] #dont do this!, there is stuff in sd1 not in 2, is 2 the sure thing? need to merg
#get the number off the end and make that a new column if they match, then plot points where the number number is the same
# merge or something???? DONT DO IT FOR NOW!!!

#to test if sd2 is a sure thing, see if anything in sd1 is NOT in sd2, ie can I just add sd1 to sd2 for anuthing that doesnt match?


# merge SD1 and SD2 -------------------------------------------------------
#for now, I have just averaged snow depth by month and winter 

#merge two dts together and remove faulty values (36 where sdepth = -9999)
snow <- rbind(sd2, sd1late)
snow <- snow[!sdepth < 0]

#create category of winter
snow[month(Date) > 10, winter := paste0(year(Date), "-", year(Date)+1)]
snow[month(Date) < 4, winter := paste0(year(Date)-1, "-", year(Date))]

#avg across locations by month and winter
crossavg <- snow[, mean(sdepth), by = .(winter, month(Date))]
setnames(crossavg, "V1", "sdepth")


#save as RDS
saveRDS(crossavg, "Data/snowdepthavg.rds")

#plot
ggplot(crossavg) +
  geom_point(aes(x = winter, y = sdepth, colour=as.factor(month)), size=3) 






###what is below here is data wrangling for camera trap data on grids



#Prep for camera snow depth -------------------------------

#add grid label for each grid snow file
ag[, where := "agnes"]
jo[, where := "jo"]
kl[, where := "kloo"]

#combine grid snow depth files
gs<-rbind(ag, jo, kl)

#convert to date
gs[, Date := dmy(DATE)]

#remove data from outside Nov1-Mar31
gs[, mnth := month(Date)] #extract month
gs1<-gs[mnth <= 3 | mnth >= 11]
#remove data from before November 1, 2015 (the cutoff date for HR analysis)
gs1<-gs1[Date >= "2015-11-01"]

#remove NA snow depths
gs1<-gs1[!is.na(`OPEN SD`)]


# Prep for combining different data types ---------------------------------

#rename cols
setnames(gs1, "OPEN SD", "sdepth")

#subset columns
gs2<-gs1[, .(Date, where, sdepth)]

#combine (fill=T so segment will be NA for grids)
snd<- rbind(dt2, gs2)


#make winter column ----------------------------------

#col for year, month
snd[, yr := year(Date)]
snd[, mnth := month(Date)]

#create first and second years for each winter
snd[mnth >=11, w1 := yr]
snd[mnth >=11, w2 := yr+1]
snd[mnth <=3, w1 := yr-1]
snd[mnth <=3, w2 := yr]

#make winter col
snd[, winter := paste0(w1, "-", w2)]

#Data exploration ------------------------------------------

#which year-months have data
snd[, count := .N, by = .(yr, mnth, where)]
snd[, countwin := .N, by = .(winter, where)]
snd[, unique(countwin), by=.(winter,where)] #important output!

#add col for if grid or transect (type)
snd[where %like% "OAH", type := "pred.trans"]
snd[is.na(type), type := "grid"]

#plot both data types together
ggplot(snd) +
  geom_point(aes(x = Date, y = sdepth, colour=where, shape=type), size=3) 
#tough becasue transects have deeper snow in 2020-21 (which we know is true)
  #but we dont know if the grid measurements also would have been high in those years
#BUT even tho limited 2015-16 transect data, it is comparable to grid data, so can use both?


#over whole project for curiosity
dtp<-dt[!is.na(SnowDepthStn)]
dtp[, mnth := month(Date)] #extract month
dtp<-dtp[mnth <= 3 | mnth >= 11]

gsp<-gs[!is.na(`OPEN SD`) & `OPEN SD` != 0]
gsp[, mnth := month(Date)] #extract month
gsp<-gsp[mnth <= 3 | mnth >= 11]

ggplot() +
  geom_point(data=dtp, aes(x = Date, y = SnowDepthStn, colour=where), size=3) +
  geom_point(data=gsp, aes(x = Date, y = `OPEN SD`, colour=where), size=3) 

