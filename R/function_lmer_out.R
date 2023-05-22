


lmer_out <- function(model) {
  
  #collect coef values
  coefOut <- data.table(t(fixef(model)))
  coefOut<-round(coefOut,2)
  
  #collect standard errors
  seOut <- data.table(t(se.fixef(model)))
  seOut<-round(seOut,2)
  
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  
  #collect R2s
  rsqOut <- data.table(r.squaredGLMM(model))
  rsqOut<- round(rsqOut, 2)
  
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}
