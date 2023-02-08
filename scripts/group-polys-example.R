# Load data.table, spatsoc
library(data.table)
library(spatsoc)

# Read example data
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))

# Cast the character column to POSIXct
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]

# EPSG code for example data
utm <- 'EPSG:32736'

# internally, build_polys

DT[, month := month(datetime)]

areas <- group_polys(
	DT,
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
	projection = utm,
	id = 'ID',
	coords = c('X', 'Y'),
	splitBy = 'month'
)
areas[, proportion := proportion / 10000]

areas_mcp <- group_polys(
		DT,
		area = TRUE,
		hrType = 'mcp',
		hrParams = list(percent = 95),
		projection = utm,
		id = 'ID',
		coords = c('X', 'Y'),
		splitBy = 'month'
	)

areas_mcp
