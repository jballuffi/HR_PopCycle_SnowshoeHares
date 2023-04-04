# #basic models for variables alone
# cycle <- lm(M90 ~ phase, dat)
# pred <- lm(M90 ~ mortrate, dat)
# comp <- lm(M90 ~ haredensity, dat)
# 
# bodysize <- lm(M90 ~ Weight, dat)
# snow <- lm(M90 ~ SD, dat)
# food <- lm(M90 ~ Food, dat)
# 
# #all resources plus demographic data
# res <- lm(M90 ~ Weight + SD + Food, dat)
# pred_res <- lm(M90 ~ mortrate + Weight + SD + Food, dat)
# comp_res <- lm(M90 ~ haredensity + Weight + SD + Food, dat)
# cycle_res <- lm(M90 ~ phase + Weight + SD + Food, dat)
# 

library(ggdag)
library(dagitty)


# how does pop dens impact hr size in a cyclic pop

dag <- dagify(
  Hr ~ Phase + Mort + Dens + Wgt + Snow + Food + Shrub + Lynx,
  
  Phase ~ Mort + Dens,
  
  Dens ~ Mort,
  
  Mort ~ Wgt + Snow + Lynx,
  
  Wgt ~ Dens + Phase + Shrub,
  
  latent = c('Shrub', 'Lynx'),
  
  outcome = 'Hr'
)
dag |> ggdag_status(seed = 2) + theme_dag_blank()


# how does resource avail and energetic conditions impact hr size 
# within phase 
dag <- dagify(
  Hr ~ Wgt + Snow + Food + Mort + Dens,
  
  Wgt ~ Food,
  
  outcome = 'Hr'
)
dag |> ggdag_status(seed = 2) + theme_dag_blank()



# https://www.youtube.com/watch?v=mBEA7PKDmiY&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=5

# https://www.youtube.com/watch?v=uanZZLlzKHw&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=6