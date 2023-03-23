

#data is GPS

test <- gps[, .(id, x_proj, y_proj, winter, weekdate, datetime)]

areas <- group_polys(
  test,
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
  test,
  area = TRUE,
  hrType = 'mcp',
  hrParams = list(percent = 90),
  projection = utm7N,
  id = 'id',
  coords = c('x_proj', 'y_proj'),
  splitBy = 'weekdate'
)

areas_mcp
