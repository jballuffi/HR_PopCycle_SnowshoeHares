# Targets workflow


# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')



# Variables ---------------------------------------------------------------
data_path <- if (Sys.info()['sysname'] == 'Windows') {
  # 'E:/HR_data'
  'data/Cleaned_GPS'
} else {
  message('set your data path')
}

utm7N <- '+proj=utm +zone=7 ellps=WGS84'

# Split by iteration 
split_by <- c('ID', 'year')


# Targets workflow --------------------------------------------------------
c(
  tar_files(
    gps_files,
    list_gps_files(data_path)
  ),
  tar_target(
    gps,
    fread(gps_files),
    pattern = map(gps_files)
  ),
  tar_target(
    gps_by,
    gps[, tar_group := .GRP, split_by],
    iteration = "group"
  ),
  tar_target(
    areas,
    mcp_area(gps_by, 'x.utm', 'y.utm', c('ID', 'year', 'datetime'), utm7N),
    pattern = map(gps_by)
  )
)

