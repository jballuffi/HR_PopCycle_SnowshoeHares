#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


files <- dir("data/cleaned_gps", full.names = TRUE)

ls.files<-lapply(files, FUN=function(m){
  dt<-fread(m)
  return(dt)
})

gps<-rbindlist(ls.files, fill = TRUE, use.names = TRUE)


gps[, ID := as.factor(ID)]
