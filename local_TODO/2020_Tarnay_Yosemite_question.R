# Leland Tarnay 2020-02-24 email

library(PWFSLSmoke)

# ----- Create a monitor subset for Wawona -------------------------------------

Wawona_area2017 <-
  monitor_loadAnnual(2017) %>%
  monitor_subset(stateCodes = c("CA")) %>%
  monitor_subset(tlim = c(20170508, 20171223)) %>%
  monitor_subsetByDistance(
    longitude = -119.652,
    latitude = 37.541,
    radius = 30
  )

# The result for this area is super messy:
monitor_leaflet(Wawona_area2017)

monitor_dygraph(Wawona_area2017)

# ----- Separate all individual ws_monitor objects -----------------------------

# Isolate monitors so we can combine or toss some of them
singleMonitorList <- monitor_isolate(Wawona_area2017)

names(singleMonitorList)

# ----- Review the individual monitor timeseries -------------------------------

layout(matrix(seq(6)))
for ( j in 0:3 ) {

  start <- j * 6 + 1
  end <- j * 6 + 6

  for ( i in start:end ) {
    monitor_timeseriesPlot(singleMonitorList[[i]])
    title(names(singleMonitorList)[i])
  }

}
layout(1)

# Several of these have only a few points and can be ignored
lapply(singleMonitorList, function(x) { nrow(x$data) }) %>%
  unlist() %>%
  sort()

# ----- Function to combine and collapse ---------------------------------------

combineCollapse <- function(singleMonitorList, monitorIDs, newID, siteName) {

  # New ws monitor is created by
  #  - select individual ws_monitors from singleMonitorList list
  #  - combine these into a mult-monitor ws_monitor object
  #  - collapse this into a single-monitor ws_monitor object
  ws_monitor <-
    singleMonitorList[monitorIDs] %>%
    monitor_combine() %>%
    monitor_collapse(monitorID = newID)

  # Add a siteName
  ws_monitor$meta$siteName <- siteName

  return(ws_monitor)

}

# ----- Combine/collapse to create named monitors ------------------------------

yv <- combineCollapse(
  singleMonitorList,
  c("060431001_01"),
  "yv",
  "Yosemite Valley"
)

mp <- combineCollapse(
  singleMonitorList,
  c("lon_.119.968_lat_37.488_mariposa.1000"),
  "mp",
  "Mariposa"
)

fc <- combineCollapse(
  singleMonitorList,
  c("lon_.119.627_lat_37.470_arb2.1018",
    "lon_.119.635_lat_37.473_arb2.1018"),
  "fc",
  "Fish Camp"
)

ww <- combineCollapse(
  singleMonitorList,
  c("lon_.119.658_lat_37.540_apcd.1011"),
  "ww",
  "Wawona"
)

pb <- combineCollapse(
  singleMonitorList,
  c("lon_.119.736_lat_37.464_arb2.1011"),
  "pb",
  "Ponderosa Basin"
)

bj <- combineCollapse(
  singleMonitorList,
  c("lon_.119.849_lat_37.449_arb2.1007"),
  "bj",
  "Bootjack"
)

ep <- combineCollapse(
  singleMonitorList,
  c("lon_.119.784_lat_37.675_apcd.1019",
    "lon_.119.784_lat_37.675_apcd.1011",
    "lon_.119.815_lat_37.669_apcd.1020",
    "lon_.119.815_lat_37.670_apcd.1019"
  ),
  "ep",
  "El Portal"
)

# Two short tests will be ignored
# monitor_timeseriesPlot(singleMonitorList[["lon_.119.711_lat_37.643_apcd.1020"]])
# monitor_timeseriesPlot(singleMonitorList[["lon_.119.709_lat_37.649_apcd.1019"]])

yw <- combineCollapse(
  singleMonitorList,
  c("lon_.119.702_lat_37.647_apcd.1020",
    "lon_.119.704_lat_37.647_apcd.1019",
    "lon_.119.706_lat_37.646_apcd.1020",
    "lon_.119.678_lat_37.635_apcd.1019",
    "lon_.119.692_lat_37.652_apcd.1020"
  ),
  "yw",
  "Yosemite West"
)

fr <- combineCollapse(
  singleMonitorList,
  c("lon_.119.771_lat_37.695_arb2.1016",
    "lon_.119.635_lat_37.473_arb2.1018"),
  "fr",
  "Foresta"
)

# ----- Create new, combined ws_monitor object ---------------------------------

Wawona_combined <-
  monitor_combine(list(yv, mp, fc, ww, pb, bj, ep, yw, fr))

monitor_leaflet(Wawona_combined)

monitor_dygraph(Wawona_combined)

monitorIDs <- Wawona_combined$meta$monitorID

monitorPlot_dailyBarplotFacets(
  ws_monitor = Wawona_combined,
  monitorIDs = monitorIDs,
  title = "MY TITLE",
  smooth_func = monitor_nowcast,
  smooth_args = list(),
  smooth_style=c('bars', 'points'),
  smooth_name = 'Hourly NowCast',
  theme = theme_monitors()
)


