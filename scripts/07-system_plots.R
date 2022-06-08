
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


DT <- readRDS("output/results/compileddata.rds")
densities <- readRDS("data/densities.rds")

  geom_line(aes(x = winter, y = lynxdensity, group = season, color = season))
DT[is.na(mass), mass := mean(mass)]

ppratio <- DT[, mean(ppratio), by = winter]

ggplot(ppratio)+
  geom_line(aes(y = V1, x = winter, by = winter))

ggplot(DT)+
  geom_point(aes(y = ppratio, x = winter, color = season))


summary(lm(DT$HRninety ~ DT$haredensity*DT$lynxdensity + DT$season + DT$mass))


plot(DT$HRninety/DT$mass ~ DT$ppratio)



ggplot(DT)+
  geom_point(aes(y = HRninety, x = lynxdensity))

ggplot(DT)+
  geom_point(aes(y = HRninety, x = haredensity))

ggplot(DT)+
  geom_boxplot(aes(y = HRninety, x = season))

ggplot(DT)+
  geom_point(aes(y = HRninety, x = ppratio, color = winter, shape = season), size = 2)+
  theme_minimal()

ggplot(DT)+
  geom_point(aes(y = ppratio, x = lynxdensity))

ggplot(DT)+
  geom_point(aes(y = ppratio, x = haredensity))
