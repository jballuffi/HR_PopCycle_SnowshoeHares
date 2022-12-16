#What is date range, SCRipt 2 SAYS OCT1-APR30?????????)

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
#transect snow depth
dt <- fread("data/Kluane_PredTrans_SnowDepth.csv")
#grid snow depths
ag <- fread("data/Agnes.csv")
jo <- fread("data/Jo.csv")
kl <- fread("data/Kloo.csv")

#Prep for grid snow depth -------------------------------

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


#Prep for transect snow depth -------------------------------

#add  label for transect data (just the segment because that is a unique measure and
#tells that it is from pred transect data)
dt[, where := Segment]

#convert to date
dt[, Date := dmy(DateTrans)]

#remove data from before November 1, 2015 (the cutoff date for HR analysis)
dt1<-dt[Date >= "2015-11-01"]

#remove NA snow depths 
dt1[!is.na(SnowDepthStn), unique(year(Date))] #(ask Alice about no data 2017-19) 
dt1<-dt1[!is.na(SnowDepthStn)]

#remove repeat snow measure per segment
dt2<-dt1[, unique(SnowDepthStn), by = .(Date, where)]
setnames(dt2, "V1", "sdepth")


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

