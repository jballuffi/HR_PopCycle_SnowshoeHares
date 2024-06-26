#script to collect food add buns

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#create path to all food add files
files <- dir("data/FoodAdds/", full.names = TRUE)

#fread and assign year based on file name
data = lapply(files, function(x) {
  res <- fread(x)
  res[, winter := tstrsplit(x, "/", keep = 3)] #grab year from file path
  res[, winter := gsub(".csv", "", winter)] #remove .csv from winter
  res
})

#rbindlist all food addds
foodadds <- rbindlist(data, fill = TRUE, use.names = TRUE)

#save food adds to output
saveRDS(foodadds, "data/food_adds.rds")
