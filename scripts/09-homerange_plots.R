#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# preparations ------------------------------------------------------------

#read in data
densities <- readRDS("output/results/densities.rds")
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

#remove winter with no data in densities
densities <- densities[!winter == "2021-2022"]

#Need to figure out these outliers! -Liam looked into them, summary in email
incl.out <- copy(DT)
DT <- DT[!M90 > 20] 

#reorder phase cycles
#DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#rename sex categories
DT[Sex == 1, Sex := "Male"][Sex == 2, Sex := "Female"]
#rename food categories
DT[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#set colors for cycle phases
cols <- c("increase" = "purple", "peak" = "green4", "decrease" = "orange", "low" = "black")
foodcols <- c("Food add" = "red3", "Control" = "grey40")



# exploring home range distributions --------------------------------------

(histo <- 
  ggplot(DT)+
  geom_histogram(aes(M90, fill = winter), color = "grey30", alpha = .6)+
  facet_wrap(~Food)+
  theme_minimal())



# Density over time --------------------------------------------------------

(h <- ggplot(densities)+
  geom_path(aes(x = date, y = haredensity, group = winter, color = phase))+
  scale_color_manual(values = cols)+
  labs(x = "", y = "Hares per ha", subtitle = "A")+
  theme_boxplots+
  theme(axis.text.x.bottom = element_text(size = 8)))

#other version
#pull means by year
wintermeans <- densities[, .(mean(haredensity), mean(mortrate, na.rm = TRUE), phase), by = winter]
names(wintermeans) <- c("winter", "haredensity", "mortrate", "phase")


(ggplot(wintermeans)+
    geom_path(aes(x = winter, y = haredensity, group = 1))+
    geom_point(aes(x = winter, y = haredensity, color = phase), size = 2)+
    scale_color_manual(values = cols)+
    labs(x = "", y = "Hares per ha")+
    theme_boxplots+
    theme(axis.text.x.bottom = element_text(size = 8)))



# mortality rate over time ------------------------------------------------

#cut to just the first day of any month
minmonths <- densities[, ymd(min(date)), by = .(mnth, winter)]
monthdates <- as.list(minmonths$V1)
#subset to new table
morts <- densities[date %in% minmonths$V1]
#remove NAs 
morts <- morts[!is.na(mortrate)]
setorder(morts, date)

(l <- ggplot(morts)+
  geom_path(aes(x = date, y = mortrate, group = winter, color = phase))+
  scale_color_manual(values = cols)+
  labs(x = "", y = "Probability of mortality", subtitle = "B")+
  theme_boxplots+
  theme(axis.text.x.bottom = element_text(size = 8)))

#other version
(ggplot(wintermeans)+
    geom_path(aes(x = winter, y = mortrate, group = 1))+
    geom_point(aes(x = winter, y = mortrate, color = phase), size = 2)+
    scale_color_manual(values = cols)+
    labs(x = "", y = "Mortality rate")+
    theme_boxplots+
    theme(axis.text.x.bottom = element_text(size = 8)))



# By year and phase for non-food add data ------------------------------------------------------------

(byyear <- 
   ggplot(DT)+
   geom_boxplot(aes(x = winter, y = M90, color = Food))+
   labs(y = "90% MCP area (ha)", x = "Winter", subtitle = "C")+
   scale_color_manual(values = foodcols)+
   theme_boxplots+
   theme(axis.text.x.bottom = element_text(size = 8)))



# Merge figures into one and save -----------------------------------------

fullbyyear <- ggarrange(h, l, byyear, ncol = 1, nrow = 3)


ggsave("Output/figures/sumfigure.jpeg", fullbyyear, width = 6, height = 9.5, units = "in")
ggsave("Output/figures/foodadd_histogram.jpeg", histo, width = 8, height = 5, units = "in")
