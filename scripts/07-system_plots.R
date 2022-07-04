
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
densities <- readRDS("output/results/densities.rds")



# plot showing animal densities over time ----------------------------------

#pull means by year
wintermeans <- densities[, .(mean(haredensity), mean(lynxdensity), mean(ppratio)), by = winter]
names(wintermeans) <- c("winter", "haredensity", "lynxdensity", "ppratio")


#hare density over time
(h <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = haredensity, group = 1))+
  labs(x = "", y = "Hares per 100 km2")+
  theme_densities)

#lynx density over time
(l <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = lynxdensity, group = 1))+
  labs(x = "", y = "Lynx per 100 km2")+
  theme_densities)

#pred-prey ratio over time
(pp <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = ppratio, group = 1))+
  labs(x = "Winter", y = "Lynx:Hare Ratio")+
  theme_densities)

#ggarrange all densities
(densityplots <- ggarrange(h, l, pp, ncol = 1, nrow = 3))




# predation risk over winter ---------------------------------------------------

(ppwinter <-
  ggplot(densities)+
  geom_line(aes(x = winterday, y = ppratio, color = winter, group = winter))+
  labs(y = "Lynx:Hare Ratio", x = "Days into winter")+
  theme_densities)






ggsave("output/figures/densities.jpeg", densityplots, width = 6, height = 9, units = "in")
ggsave("output/figures/ppratios.jpeg", ppwinter, width = 6, height = 4, units = "in")
