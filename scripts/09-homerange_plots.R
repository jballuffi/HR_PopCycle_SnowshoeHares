#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# preparations ------------------------------------------------------------

#read in data
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

#Need to figure out these outliers! -Liam looked into them, summary in email
incl.out <- copy(DT)
DT <- DT[!M90 > 20] 

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#rename sex categories
DT[Sex == 1, Sex := "Male"][Sex == 2, Sex := "Female"]
#rename food categories
DT[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#create 2nd data frame that has no food adds
DTnofood <- DT[Food == "Control"]

#set colors for cycle phases
cols <- c("increase" = "purple", "peak" = "green4", decrease = "orange", low = "red3")

#pull out the years with food add
foodyears <- DT[Food == "Food add", unique(winter)]

#make a data frame to only include the winters with food add 
yesfood <- DT[winter %in% foodyears]



# By year and phase for non-food add data ------------------------------------------------------------

(byyear <- 
   ggplot(DTnofood)+
   geom_boxplot(aes(x = winter, y = M90, color = phase))+
   labs(y = "90% MCP area (ha)", x = "Winter")+
   scale_color_manual(values = cols)+
   theme_boxplots+
   theme(axis.text.x.bottom = element_text(size = 8)))



# Home range in response to food add --------------------------------------

(bytreatment <- 
  ggplot(yesfood)+
  geom_boxplot(aes(x = Food, y = M90))+
  labs(y = "90% MCP area (ha)", x = "Food treatment")+
  theme_boxplots)







ggsave("Output/figures/HRbyyear.jpeg", byyear, width = 7, height = 6, units = "in")
ggsave("Output/figures/HRbytreatment.jpeg", bytreatment, width = 4, height = 5, units = "in")
