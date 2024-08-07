
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
densities <- readRDS("output/results/densities.rds")

#get phase from DT and merge into snow data
phases <- DT[, getmode(phase), winter]
setnames(phases, "V1", "phase")

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#set colors for cycle phases'
cols <- c("increase" = "purple", "peak" = "green4", "decrease" = "orange", low = "red3")



# plot showing animal densities by winter ----------------------------------

#pull means by year
wintermeans <- densities[, .(mean(haredensity), mean(mortrate, na.rm = TRUE), phase), by = winter]
names(wintermeans) <- c("winter", "haredensity", "mortrate", "phase")
#remove winter with no collar data
wintermeans <- wintermeans[!winter == "2021-2022"]

#hare density over time
(h <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = haredensity, group = 1))+
  geom_point(aes(x = winter, y = haredensity, color = phase), size = 2)+
  scale_color_manual(values = cols)+
  labs(x = "", y = "Hares per ha")+
  theme_densities)

#mort rate over time
(l <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = mortrate, group = 1))+
  geom_point(aes(x = winter, y = mortrate, color = phase), size = 2)+
  scale_color_manual(values = cols)+
  labs(x = "Winter", y = "Mortality rate (unit??)")+
  theme_densities)

#ggarrange all densities
(densityplots <- ggarrange(h, l, ncol = 1, nrow = 2))


ggsave("output/figures/densities.jpeg", densityplots, width = 8, height = 9, units = "in")
