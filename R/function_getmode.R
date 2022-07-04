#function to calculate mode

#use like you would the mean() function. Only argument is v- an object or string

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
