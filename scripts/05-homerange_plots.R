
#script for making descriptive figures. Figure 1 in manuscript.

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# preparations ------------------------------------------------------------

#read in data
densities <- readRDS("data/densities.rds")
DT <- readRDS("data/compileddata.rds")

#remove winter with no HR data
densities <- densities[!winter == "2021-2022"]

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#rename sex categories
DT[Sex == 1, Sex := "Male"][Sex == 2, Sex := "Female"]
#rename food categories
DT[Food == 1, Food := "Suppl."][Food == 0, Food := "Control"]

#set colors for cycle phases
cols <- c("increase" = "purple", "peak" = "green4", "decrease" = "orange", "low" = "black")
foodcols <- c("Suppl." = "red3", "Control" = "grey40")

#calculate the mean densityby winter
meandens <- densities[, .(mean = mean(haredensity), date = mean(date)), winter]



# exploring home range distributions --------------------------------------

(histo <- 
  ggplot(DT)+
  geom_histogram(aes(M90, fill = winter), color = "grey30", alpha = .6)+
  facet_wrap(~Food)+
  theme_minimal())



# Multi-panel summary figure --------------------------------------------------------

(d <- ggplot(densities)+
  geom_path(aes(x = date, y = haredensity, group = winter, color = phase), data = densities)+
  geom_point(aes(x = date, y = mean), data = meandens)+
  scale_color_manual(values = cols, breaks=c('increase', 'peak', 'decrease', 'low'), name = "Phase")+
  labs(x = "", y = "Hare density (hares/ha)", subtitle = "A)")+
  themethesisright)

(f <- 
    ggplot(DT)+
    geom_boxplot(aes(x = winter, y = M90, fill = Food), alpha = .5)+
    labs(y = "90% MCP area (ha)", x = "Winter", subtitle = "B)")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    themethesisright)

(s <- 
  ggplot(DT[!is.na(season)])+
  geom_boxplot(aes(x = winter, y = M90, linetype = season))+
  labs(y = "90% MCP area (ha)", x = "Winter", subtitle = "C)")+
  scale_linetype(name = "Season")+
  themethesisright)


fullbyyear <- ggarrange(d, f, s, ncol = 1, nrow = 3)




# FOR TALKS Multi-panel summary figure --------------------------------------------------------

density_avg <- densities[, .(haredensity = mean(haredensity)), by = .(winter, phase)]

(dT <- ggplot(density_avg)+
   geom_line(aes(x = winter, y = haredensity, group = 1, color = phase), linewidth = 1)+
   scale_color_manual(values = cols, breaks=c('increase', 'peak', 'decrease', 'low'))+
   labs(y = "Hare density (hares/ha)", x = "Winter", subtitle = "A)")+
   themethesisright)

(fT <- 
    ggplot(DT)+
    geom_boxplot(aes(x = winter, y = M90, fill = Food), alpha = 0.5)+
    labs(y = "HR area (ha)", x = "Winter", subtitle = "B)")+
    scale_fill_manual(values = foodcols)+
    themethesisright)

(sT <- 
    ggplot(DT[!is.na(season)])+
    geom_boxplot(aes(x = winter, y = M90, linetype = season))+
    labs(y = "HR area (ha)", x = "Winter", subtitle = "C")+
    scale_linetype(name = "Season")+
    themethesisright)


fortalks <- ggarrange(d, fT, sT, ncol = 1, nrow = 3)




# save -----------------------------------------

ggsave("Output/figures/sumfigure.jpeg", fullbyyear, width = 6, height = 9.5, units = "in")
ggsave("Output/figures/sumfigurefortalks.jpeg", fortalks, width = 6, height = 9.5, units = "in")

ggsave("Output/figures/foodadd_histogram.jpeg", histo, width = 8, height = 5, units = "in")

