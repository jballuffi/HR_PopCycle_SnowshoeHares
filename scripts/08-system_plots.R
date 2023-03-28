
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
densities <- readRDS("output/results/densities.rds")
snow <- readRDS("data/snowgrids.rds")

#get phase from DT and merge into snow data
phases <- DT[, getmode(phase), winter]
setnames(phases, "V1", "phase")
snow <- merge(snow, phases, by = "winter", all.x = TRUE)
snow <- snow[!winter == "2014-2015"]



# plot showing animal densities by winter ----------------------------------

#pull means by year
wintermeans <- densities[, .(mean(haredensity), mean(lynxdensity), mean(ppratio), phase), by = winter]
names(wintermeans) <- c("winter", "haredensity", "lynxdensity", "ppratio", "phase")


#hare density over time
(h <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = haredensity, group = 1))+
  geom_point(aes(x = winter, y = haredensity, color = phase), size = 2)+
  labs(x = "", y = "Hares per 100 km2")+
  theme_densities)

#lynx density over time
(l <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = lynxdensity, group = 1))+
  geom_point(aes(x = winter, y = lynxdensity, color = phase), size = 2)+
  labs(x = "", y = "Lynx per 100 km2")+
  theme_densities)

#pred-prey ratio over time
(pp <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = ppratio, group = 1))+
  geom_point(aes(x = winter, y = ppratio, color = phase), size = 2)+
  labs(x = "Winter", y = "Lynx:Hare Ratio")+
  theme_densities)

#ggarrange all densities
(densityplots <- ggarrange(h, l, pp, ncol = 1, nrow = 3))



# Plot resource variables by winter ---------------------------------------


(mass <- ggplot(DT)+
  geom_boxplot(aes(x = winter, y = mass, color = phase))+
  labs(x = "Winter", y = "Body mass (g)")+
  theme_densities)

(sd <- ggplot(snow)+
  geom_boxplot(aes(x = winter, y = SD, color = phase))+
  labs(x = "Winter", y = "Snow depth (cm)")+
  theme_densities)

(resourceplots <- ggarrange(mass, sd, ncol = 1, nrow = 2))


# predation risk over winter ---------------------------------------------------

(ppwinter <-
  ggplot(densities)+
  geom_line(aes(x = winterday, y = ppratio, color = phase, group = winter), linewidth = .8)+
  labs(y = "Lynx:Hare Ratio", x = "Days into winter")+
  theme_densities)





ggsave("output/figures/densities.jpeg", densityplots, width = 6, height = 9, units = "in")
ggsave("output/figures/resource.jpeg", resourceplots, width = 6, height = 8, units = "in")
ggsave("output/figures/ppratios.jpeg", ppwinter, width = 6, height = 4, units = "in")
