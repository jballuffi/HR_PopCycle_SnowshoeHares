#function that calculates home range sizes for varing levels of sample effort (days)
# function is dependent on the mcp_area function
# argument: DT = the data.table with gps locs

area_asym <- function(DT){
  
  #declare sample effort categories (count of days)
  effort <- c(5, 9, 13, 17, 21, 25, 29, 33, 37)
  
  #apply the mcp_area function to calculate area of home range to list of sample efforts
  hrs <- lapply(effort, function(n) {
    mcp_area(DT[diffday < n], x = 'x.utm', y = 'y.utm', utmzone = utm7N)
  }) 
  
  maxDD <- DT[, max(diffday)]
  
  #rbindlist output
  hrsDT <- rbindlist(hrs)
  
  #rename column as area
  names(hrsDT) <- "area"

  #add in column showing sample effort (count of days)
  hrsDT[, daycount := effort]
  
  #add in column with the max diff day
  hrsDT[, maxdiffday := maxDD]
  
  #return data.table
  return(hrsDT)

}
