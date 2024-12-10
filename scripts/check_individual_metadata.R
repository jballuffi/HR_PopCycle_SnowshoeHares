#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

deploy <- data.table::fread("data/compiled 2015-22_collarmastersheet.csv", header = TRUE)
foods <- readRDS("data/food_adds.rds")

foods[, Eartag := as.character(Eartag)]

deploy[, start_date := as.Date(`Date on Bunny`, format = '%d-%b-%y')]
deploy[, end_date := as.Date(`Date Off Bunny`, format = '%d-%b-%y')]
deploy[is.na(end_date), end_date := start_date + 21 ]
deploy[, Eartag := `Bunny ID`]

deploy[, Eartag := as.character(gsub('B', '2', Eartag))]

deploy[, start_date := start_date + 1]
deploy[, end_date := end_date - 1]

deploy[, mnth := month(start_date)]
deploy[, yr := year(start_date)]
deploy[mnth > 6, winter := paste0(yr, "-", yr+1)]
deploy[mnth < 6, winter := paste0(yr-1, "-", yr)]


deploy <- merge(deploy, foods, by = c("Eartag", "winter"), all.x = TRUE)
deploy[is.na(Food), Food := 0]
deploy[, Food := as.factor(Food)]

deploy <- deploy[mnth == 12|mnth < 4]

nsize <- deploy[, .N, by = .(winter, Food)]




deploy[, deploy_id := paste0(bunny, '_', start_date), .(bunny, start_date)]




dates <- tar_read(dates)




table(unique(dates$animal) %in% unique(deploy$bunny))
deploy[is.na(start_date)]
