

#data is GPS



areas <- group_polys(
  gps,
  area = TRUE,
  hrType = 'mcp',
  hrParams = list(
    h = "href",
    grid = 200,
    same4all = TRUE,
    hlim = c(0.1, 1.5),
    kern = c("bivnorm"),
    extent = 0.5
  ),
  projection = utm7N,
  id = 'id',
  coords = c('x_proj', 'y_proj'),
  splitBy = 'winter'
)
areas[, proportion := proportion / 10000]

areas_mcp <- group_polys(
  gps,
  area = TRUE,
  hrType = 'mcp',
  hrParams = list(percent = 90),
  projection = utm7N,
  id = 'id',
  coords = c('x_proj', 'y_proj'),
  splitBy = 'weekdate'
)

areas_mcp
