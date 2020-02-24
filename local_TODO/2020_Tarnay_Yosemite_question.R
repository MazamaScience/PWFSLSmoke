# Leland Tarnay 2020-02-24 email

library(PWFSLSmoke)


# ???
statesList <- c("CA")

# All monitors for 2017
allmon_canv2017 <-
  monitor_loadAnnual(2017) %>%
  monitor_subset(stateCodes = statesList) %>%
  monitor_subset(tlim = c(20170508, 20171223))

Wawona_area2017 <-
  allmon_canv2017 %>%
  monitor_subsetByDistance(
    longitude = -119.652,
    latitude = 37.541,
    radius = 30
  )

# The result for this area is super messy:
monitor_leaflet(Wawona_area2017)
monitor_dygraph(Wawona_area2017)

# Isolate monitors so we can combine some of them

combineCollapse <- function(singleMonitors, monitorIDs, newID, siteName) {

  # New ws monitor is created by
  #  - select individual ws_monitors from singleMonitors list
  #  - combine these into a mult-monitor ws_monitor object
  #  - collapse this into a single-monitor ws_monitor object
  ws_monitor <-
    singleMonitors[monitorIDs] %>%
    monitor_combine() %>%
    monitor_collapse(monitorID = newID)

  # Add a siteName
  ws_monitor$meta$siteName <- siteName

  return(ws_monitor)

}

singleMonitors <- monitor_isolate(Wawona_area2017)

# Quick look at all of them
layout(matrix(seq(6)))
for ( j in 0:3 ) {

  start <- j * 6
  end <- j * 6 + 5

  for ( i in start:end ) {
    monitor_timeseriesPlot(singleMonitors[[i]])
    title(names(singleMonitors)[i])
  }

}
layout(1)

yv <- combineCollapse(
  singleMonitors,
  c("060431001_01"),
  "yv",
  "Yosemite Valley"
)

mp <- combineCollapse(
  singleMonitors,
  c("lon_.119.968_lat_37.488_mariposa.1000"),
  "mp",
  "Mariposa"
)

fc <- combineCollapse(
  singleMonitors,
  c("lon_.119.627_lat_37.470_arb2.1018",
    "lon_.119.635_lat_37.473_arb2.1018"),
  "fc",
  "Fish Camp"
)

ww <- combineCollapse(
  singleMonitors,
  c("lon_.119.658_lat_37.540_apcd.1011"),
  "ww",
  "Wawona"
)

pb <- combineCollapse(
  singleMonitors,
  c("lon_.119.736_lat_37.464_arb2.1011"),
  "pb",
  "Ponderosa Basin"
)

bj <- combineCollapse(
  singleMonitors,
  c("lon_.119.849_lat_37.449_arb2.1007"),
  "bj",
  "Bootjack"
)

# Two short tests will be ignored
monitor_timeseriesPlot(singleMonitors[["lon_.119.815_lat_37.669_apcd.1020"]])
monitor_timeseriesPlot(singleMonitors[["lon_.119.709_lat_37.649_apcd.1019"]])

ep <- combineCollapse(
  singleMonitors,
  c("lon_.119.784_lat_37.675_apcd.1019",
    "lon_.119.784_lat_37.675_apcd.1011",
    ###"lon_.119.815_lat_37.669_apcd.1020",
    "lon_.119.815_lat_37.670_apcd.1019",
    ###"lon_.119.815_lat_37.669_apcd.1020"
  ),
  "ep",
  "El Portal"
)

# Two short tests will be ignored
# monitor_timeseriesPlot(singleMonitors[["lon_.119.711_lat_37.643_apcd.1020"]])
# monitor_timeseriesPlot(singleMonitors[["lon_.119.709_lat_37.649_apcd.1019"]])

yw <- combineCollapse(
  singleMonitors,
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
  singleMonitors,
  c("lon_.119.771_lat_37.695_arb2.1016",
    "lon_.119.635_lat_37.473_arb2.1018"),
  "fr",
  "Foresta"
)


Wawona_combined <-
  monitor_combine(list(yv, mp, fc, ww, pb, bj, ep, yw, fr))

################################################################################
################################################################################
################################################################################






# Yosemite Valley
yv <- singleMonitors[["060431001_01"]]

# Mariposa
mp <- singleMonitors[["lon_.119.968_lat_37.488_mariposa.1000"]]

# Fish Camp
fc <-
  monitor_combine(list(
    singleMonitors[["lon_.119.627_lat_37.470_arb2.1018"]],
    singleMonitors[["lon_.119.635_lat_37.473_arb2.1018"]]
  )) %>%
  monitor_collapse(monitorID = "Fish Camp")

# Wawona
ww <- singleMonitors[["lon_.119.658_lat_37.540_apcd.1011"]]

# Ponderosa Basin
pb <- singleMonitors[["lon_.119.736_lat_37.464_arb2.1011"]]

# Bootjack
bj <- singleMonitors[["lon_.119.849_lat_37.449_arb2.1007"]]

# El POrtal
fc <-
  monitor_combine(list(
    singleMonitors[["lon_.119.627_lat_37.470_arb2.1018")]],
    singleMonitors[["lon_.119.635_lat_37.473_arb2.1018"]]
  )) %>%
  monitor_collapse(monitorID = "Fish Camp")



# By doing this (which took a while):

#first need to combine multiple duplicates

#Yosemite Valley
yv <- monitor_subset(Wawona_area2017, monitorIDs=("060431001_01"))

#mapirposa
mp <-monitor_subset(Wawona_area2017, monitorIDs=( "lon_.119.968_lat_37.488_mariposa.1000"))

#fish camp
fc1 <-monitor_subset(Wawona_area2017, monitorIDs=( "lon_.119.627_lat_37.470_arb2.1018"))
fc2 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.635_lat_37.473_arb2.1018"))
fc_all <- monitor_combine(list(fc1,fc2))
fc <- monitor_collapse(fc_all, monitorID = "Fish Camp")

#Wawona
ww1 <-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.658_lat_37.540_apcd.1011"))
ww <- monitor_collapse(ww1, monitorID = "Wawona-Chilnualna Falls Road")

#5PONDEROSA BASIN
pbasin1 <-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.736_lat_37.464_arb2.1011"))
pb <- monitor_collapse(pbasin1, monitorID = "Ponderosa Basin")

#BOOTJACK rename
bootjack1 <-monitor_subset(Wawona_area2017, monitorIDs=( "lon_.119.849_lat_37.449_arb2.1007"))
bootjack<- monitor_collapse(bootjack1, monitorID = "Bootjack")

#6EL PORTAL
elportal1<-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.784_lat_37.675_apcd.1019"))
elportal2<-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.784_lat_37.675_apcd.1011"))
elportal3<-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.815_lat_37.669_apcd.1020"))
elportal4<-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.815_lat_37.670_apcd.1019"))
elportal5<-monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.815_lat_37.669_apcd.1020"))
ep_ <- monitor_combine(list(elportal1,elportal2, elportal3, elportal4, elportal5))
ep <- monitor_collapse(ep_, monitorID = "El Portal")
monitor_dygraph(ep)
monitor_leaflet(ep)

#8YOSEMITE WEST
yosewest1 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.711_lat_37.643_apcd.1020"))#ghost?
yosewest2 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.709_lat_37.649_apcd.1019"))#ghost?
yosewest3 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.702_lat_37.647_apcd.1020"))
yosewest4 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.704_lat_37.647_apcd.1019"))
yosewest5 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.706_lat_37.646_apcd.1020"))
yosewest6 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.678_lat_37.635_apcd.1019"))
yosewest7 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.692_lat_37.652_apcd.1020"))
yw_all <- monitor_combine(list(yosewest1, yosewest2, yosewest3, yosewest4, yosewest5, yosewest6, yosewest7))
yw <- monitor_collapse(yw_all, monitorID = "Yosemite West")
monitor_dygraph(yw)

#7FORESTA
foresta1 <-monitor_subset(Wawona_area2017, monitorIDs=( "lon_.119.771_lat_37.695_arb2.1016"))
foresta2 <- monitor_subset(Wawona_area2017, monitorIDs=("lon_.119.754_lat_37.700_arb2.1016"))
frsta_all <- monitor_combine(list(foresta1,foresta2))
frsta <- monitor_collapse(frsta_all, monitorID = "Foresta")

ww17 <- monitor_combine(list(bootjack,ep,frsta,yw,pb,fc, ww, mp, yv))

# So, while this all results in what I thought was a monitor object that actually even maps when you use leaflet, it’s actually over in the sidebar as a “list of 2” instead of a monitor object, so something’s screwy with how I built these things I think…what am I missing?
#
#   I have the talk tomorrow night in Yosemite so if you have a minute to set me straight would appreciate! I have 6 hours on the road today, minimum, so will try calling a bit later…Thanks!
#
#   Lee
