library(PWFSLSmoke)

# All of AIRSIS for 2019
airsis <- airsis_loadAnnual(2019)

# AIRSIS in California
#  - start with airsis
#  - subset for those where stateCode is one of "CA"
airsis_ca <- 
  airsis %>%
  monitor_subset(stateCodes = ("CA"))

# Show the siteNames and IDs
#  - start with airsis_ca
#  - extract the "meta" dataframe
#  - select the "siteName column
#  - (print by default)
airsis_ca %>%
  monitor_extractMeta() %>%
  dplyr::select(siteName)

# Interactive map to picak a monitor
monitor_leaflet(airsis_ca)

# Select a single monitor by monitorID
#  - start with airsis_ca
#  - subset for the "Mariposa-5085 Bullion St" monitorID
Mariposa <-
  airsis_ca %>%
  monitor_subset(monitorIDs = "lon_.119.968_lat_37.488_mariposa.1000")

# Interactive graph to pick some time limits
monitor_dygraph(Mariposa)

# Trim this time series to October through December
#  - start with Mariposa
#  - subset based on time limits
Mariposa <- 
  Mariposa %>%
  monitor_subset(tlim = c(20191001, 20200101))

# A quick plot for October through December
monitor_timeseriesPlot(Mariposa)
addAQILines()
addAQIStackedBar()
addAQILegend("topright")
title("Mariposa 2019")

# Dump out a meta/data combined CSV file for Mariposa
monitor_writeCSV(
  Mariposa, 
  saveFile = "Mariposa.csv",
  metaOnly = FALSE,
  dataOnly = FALSE,
  quietly = TRUE
)

# Alternatively, View the metadata and data in RStudio:
View(Mariposa$meta)
View(Mariposa$data)

# Trim the all-California dataset to the period that has data
airsis_ca <- monitor_trim(airsis_ca)

# Dump out separate meta and data CSV files for airsis_ca
monitor_writeCSV(
  airsis_ca, 
  saveFile = "airsis_CA_meta.csv",
  metaOnly = TRUE,
  dataOnly = FALSE,
  quietly = TRUE
)

monitor_writeCSV(
  airsis_ca, 
  saveFile = "airsis_CA_data.csv",
  metaOnly = FALSE,
  dataOnly = TRUE,
  quietly = TRUE
)

# Alternatively, View the metadata and data in RStudio:
View(airsis_ca$meta)
View(airsis_ca$data)

# ==============================================================================

# Everything abvoe also applies to temporary data from the Western Regional
# Climate Center. Just start with:

# All of WRCC for 2019
wrcc <- wrcc_loadAnnual(2019)



