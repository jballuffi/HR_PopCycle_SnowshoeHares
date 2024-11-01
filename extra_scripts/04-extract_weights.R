
# prep and import ---------------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#import trapping data
trapping <- fread("data/Trapping_data_all_records.csv")
#for sex column, 1 = male, 2 = female, 0 = no data #these are correct codes; LGH checked Apr26/2023

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")


# clean trapping data -----------------------------------------------------

#make eartag a factor
trapping[, id := as.factor(Eartag)]

#turn 0s to NAs
trapping[Sex == 0, Sex := NA]
#get mode of sex by id to remove any erroenous sex, this function doesnt account for NAs
trapping[, Sex := getmode(Sex), by = id]
#change to factor
trapping[, Sex := as.factor(Sex)]


#grab all unique individuals from home range calculations
inds <- unique(areas$id)

#subset the trapping data.table to only include these individuals
# also subset to remove all cases where weight was not taken
t2 <- trapping[id %in% inds & Weight > 0]

#turn dateCap column into a date with lubridate function
t2[, date := dmy(dateCap)]

#take out month from date column
t2[, mnth := month(date)]

#categorize fixes into winters
t2[mnth > 9, winter := paste0(year(date), "-", year(date)+1)] #grab october because we miss lots of weights otherwise
t2[mnth < 4, winter := paste0(year(date)-1, "-", year(date))]

#remove anything that doesn't fall within winter
t2 <- t2[!is.na(winter)]



# Get weight by winter ----------------------------------------------------

# get avg winter weights by individual, paired with sex
weightbywinter <- t2[, .(mean(Weight), getmode(Sex), .N), by = .(id, winter)]
setnames(weightbywinter, c("V1", "V2"), c("mass", "sex"))



saveRDS(t2, "output/results/bodymass_allweights.rds")
saveRDS(weightbywinter, "output/results/bodymass_bywinter.rds")
