# Function to collect coefficients, standard errors, and R2s for linear models
# 

lm_out <- function(model) {
  #summarize model
  out <- summary(model)
  
  #collect coef values
  coefOut <- data.table(t(out$coefficients[, 1]))
  coefOut<-round(coefOut, 3)
  
  #collect standard errors
  seOut <- data.table(t(out$coefficients[, 2]))
  seOut<-round(seOut, 3)
  
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  rsqOut <- round(rsqOut, 3)
  
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}
