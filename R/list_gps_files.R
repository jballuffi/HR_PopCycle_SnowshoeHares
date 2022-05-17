list_gps_files <- function(path) {
  all_paths <- dir(path, pattern = '*.csv', recursive = TRUE, full.names = TRUE)
  
  grep('XYZ|error', all_paths, value = TRUE)
}