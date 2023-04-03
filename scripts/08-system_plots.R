
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

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#set colors for cycle phases
cols <- c("increase" = "purple", "peak" = "green4", decrease = "orange", low = "red3")

# plot showing animal densities by winter ----------------------------------

#pull means by year
wintermeans <- densities[, .(mean(haredensity), mean(mortrate, na.rm = TRUE), phase), by = winter]
names(wintermeans) <- c("winter", "haredensity", "mortrate", "phase")


#hare density over time
(h <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = haredensity, group = 1))+
  geom_point(aes(x = winter, y = haredensity, color = phase), size = 2)+
  scale_color_manual(values = cols)+
  labs(x = "", y = "Hares per 100 km2")+
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



# Plot resource variables by winter ---------------------------------------


(mass <- ggplot(DT)+
  geom_boxplot(aes(x = winter, y = Weight, color = phase))+
  scale_color_manual(values = cols)+
  labs(x = "Winter", y = "Body mass (g)")+
  theme_boxplots)

(sd <- ggplot(snow)+
  geom_boxplot(aes(x = winter, y = SD, color = phase))+
  scale_color_manual(values = cols)+
  labs(x = "Winter", y = "Snow depth (cm)")+
  theme_boxplots)

(resourceplots <- ggarrange(mass, sd, ncol = 1, nrow = 2))






ggsave("output/figures/densities.jpeg", densityplots, width = 8, height = 9, units = "in")
ggsave("output/figures/resource.jpeg", resourceplots, width = 8, height = 9, units = "in")
