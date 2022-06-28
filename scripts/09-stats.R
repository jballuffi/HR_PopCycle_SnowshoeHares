
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

data <- readRDS("output/results/compileddata.rds")


summary(lm(HRninety ~ Food + haredensity + lynxdensity + mass, data))


plot(data$HRninety ~ data$lynxdensity)
