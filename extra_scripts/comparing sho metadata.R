#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in prepped GPS data
ours <- fread("data/compiled 2015-22_collarmastersheet.csv")
ours[, source := "us"]
sm1518 <- fread("data/liam_test_data/Sho2015-18_Mikesheet.csv")
sm1518[, source:="mike"]
sy1518 <- fread("data/liam_test_data/Sho2015-18_Yassheet.csv")
sy1518 <- sy1518[, 1:28]
sy1518[, source := "yas"]
# sr1518 <- fread("data/liam_test_data/Sho2015-18_rolosheet.csv") #this looks like just AXY data

s1819 <- fread("data/liam_test_data/Sho2018-19.csv")
s1819[, source := "sho"]


#convert to date based on Date GPS On
ours[, Date := lubridate::dmy(`Date GPS On`)]
ours[, yr := lubridate::year(Date)]
ours[, mnth := lubridate::month(Date)]
#keep 3 NAs bc maybe just the metadata missing
ours19<-ours[yr < 2020 | is.na(yr)] #remove after 2019
ours19<-ours19[!(yr == 2019 & mnth >9) | is.na(yr)]


#compiling sho's data because there were multiple sheets
smy1518<-rbind(sm1518, sy1518)
all<-rbind(smy1518, s1819)

mys<-all[, .(`Bunny ID`, GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`, source)]
#remove non deployed collars
mys[, unique(`Bunny ID`)]
mys <- mys[!is.na(`Bunny ID`)]
mys<-mys[`Bunny ID` != "Not Deployed"]

#check for duplicates
mys[, dup:=duplicated(`Bunny ID`), by=.(GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`)]
mys[, unique(dup)] # no duplicates


#remove rows with no GPS
mys<-mys[`Date GPS On` != "-"] #some say AUDIO as the GPS ID but have a Date GPS On
mys<-mys[!(`Date GPS On` == "" & GPS=="")]
mys[is.na(GPS), GPS := "NoIDListed"] #no GPS ID, but has a date gps on
mys<-mys[, .(`Bunny ID`, GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`, source)] #remove dup cols
setnames(mys, "Bunny ID", "id")

o19<-ours19[, .(`Bunny ID`, GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`, source)]
o19[is.na(GPS), GPS := "NoIDListed"]#no GPS ID, but has a date gps on
setnames(o19, "Bunny ID", "id")

#make data formats consistent
o19[, id := gsub('B', '2', id)]
o19[, start.date := lubridate::dmy(`Date on Bunny`)]
o19[, end.date := lubridate::dmy(`Date Off Bunny`)]
o19[, date.gps.on := lubridate::dmy(`Date GPS On`)]
o19<-o19[, .(id, GRID, GPS, start.date, end.date, date.gps.on, source)] #get renamed cols


mys[, start.date := lubridate::dmy(`Date on Bunny`)]
mys[, end.date := lubridate::dmy(`Date Off Bunny`)]
mys[, date.gps.on := lubridate::dmy(`Date GPS On`)]
mys<-mys[, .(id, GRID, GPS, start.date, end.date, date.gps.on, source)] #get renamed cols


#test if our and theirs line up
com2<-merge(o19, mys, by=c("id", "start.date", "end.date", "date.gps.on", "GPS", "GRID"), all=T)
good<-com2[!is.na(source.x) & !is.na(source.y)]
bad<-com2[is.na(source.x) | is.na(source.y)]


#all this really means is that the metadata sho just gave us isn't quite the same as what mike and emily gave us.
#and for any slight discrepancies in the "bad", we probably side with sho's data bc it's published? 
#We can also cross reference with the actual data when there is a big discrepancy like 1 month,
#it could be that our data acknowledges a mortality for the end data or something that sho was unaware of
