#function that calculates home range sizes for varing levels of sample effort (days)
# function is dependent on the mcp_area function
# argument: DT = the data.table with gps locs

area_asym <- function(DT){
  
  effort <- c(8, 15, 22, 29, 36)
  
  hrs <- lapply(effort, function(n) {
    mcp_area(DT[diffday < n], x = 'x.utm', 'y.utm', 'winter', utm7N)
  })
}