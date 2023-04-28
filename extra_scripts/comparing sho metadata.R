#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in prepped GPS data
ours <- fread("data/compiled 2015-22_collarmastersheet.csv")
sm1518 <- fread("data/liam_test_data/Sho2015-18_Mikesheet.csv")
sm1518[, source:="mike"]
sy1518 <- fread("data/liam_test_data/Sho2015-18_Yassheet.csv")
sy1518 <- sy1518[, 1:28]
sy1518[, source := "yas"]
sr1518 <- fread("data/liam_test_data/Sho2015-18_rolosheet.csv") #this looks like just AXY data

s1819 <- fread("data/liam_test_data/Sho2018-19.csv")
s1819[, source := "sho"]


#convert to date
ours[, Date := lubridate::dmy(`Date GPS On`)]
ours[, yr := lubridate::year(Date)]
ours<-ours[yr <=2019]
ours[, source := "us"]


#just trying to get sho's data because there were multiple sheets
smy1518<-rbind(sm1518, sy1518)
all<-rbind(smy1518, s1819)

mys<-all[, .(`Bunny ID`, GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`, source)]
mys[, dup:=duplicated(`Bunny ID`), by=.(GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`)]
mys <- my[!is.na(`Bunny ID`)]
mys[, unique(dup)]

#remove non deployed collars
mys[, unique(`Bunny ID`)]
mys<-mys[`Bunny ID` != "Not Deployed"]

#remove rows with no GPS
mys<-mys[`Date GPS On` != "-"] #some say AUDIO as the GPS ID
mys<-mys[`Date GPS On` != ""]
mys[, dup2:=duplicated(`Bunny ID`), by=.(GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`)]
mys[, unique(dup2)]
mys[is.na(GPS), GPS := "NoID"]
mys<-mys[, .(`Bunny ID`, GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`, source)]


mys[, unique(GRID)]
ours[, unique(GRID)]

sub.ours<-ours[, .(`Bunny ID`, GRID, GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`, source)]
sub.ours[is.na(GPS), GPS := "NoID"]

#test if our and theirs line up
com<-rbind(sub.ours, mys)
com[, dup:=duplicated(`Bunny ID`), by=.(GPS, `Date GPS On`, `Date on Bunny`, `Date Off Bunny`)]

com2<-merge(sub.ours, mys, by=c("Bunny ID", "Date GPS On", "Date on Bunny", "Date Off Bunny"), all=T)
com2[is.na(source.x) | is.na(source.y)]
com2[is.na(source.x)]
com2[is.na(source.y)]
