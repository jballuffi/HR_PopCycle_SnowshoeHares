
# prep and import ---------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#import trapping data
trapping <- fread("data/Trapping_data_all_records.csv")

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")




# clean trapping data -----------------------------------------------------

#make eartag a factor
trapping[, id := as.factor(Eartag)]

#grab all unique individuals from home range calculations
inds <- unique(areas$id)

#subset the trapping data.table to only include these individuals
# also subset to remove all cases where weight was not taken
t2 <- trapping[id %in% inds & Weight > 0]

#turn dateCap column into a date with lubridate function
t2[, date := dmy(dateCap)]

#categorize gps fixes into winters,
#grab all of october because that's when a lot of trapping started
#should prob turn this into a function
t2[date > "2015-10-01" & date < "2016-04-01", winter := "2015-2016"]
t2[date > "2016-10-01" & date < "2017-04-01", winter := "2016-2017"]
t2[date > "2017-10-01" & date < "2018-04-01", winter := "2017-2018"]
t2[date > "2018-10-01" & date < "2019-04-01", winter := "2018-2019"]

#remove anything that doesn't fall into winter
t2<- t2[!is.na(winter)]

#take out month from date column
# t2[, mnth := month(date)]

#create early winter and late winter categories
#fixes in nov and dec are "early"
#fixes in feb and march are "late"
t2[mnth == 10| mnth == 11| mnth == 12, season := "early"]
t2[mnth == 2| mnth == 3, season := "late"]

#remove anything that isn't in early winte or late winter
t2 <- t2[!is.na(season)]

ggplot(t2)+
  geom_boxplot(aes(x = season, y = Weight))
summary(lm(t2$Weight ~ t2$season))



weights <- t2[, mean(Weight), by = .(id, winter, season)]
setnames()




#average mass for early winter season (November and December)


#average mass for late winter season (February and March)
