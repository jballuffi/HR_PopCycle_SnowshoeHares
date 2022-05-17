# Targets workflow


# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')



# Variables ---------------------------------------------------------------
data_path <- if (Sys.info()['sysname'] == 'Windows') {
  'E:/HR_data'
} else {
  message('set your data path')
}


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
    rows,
    nrow(gps)
  )
)