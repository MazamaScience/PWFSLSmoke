# Katy Warner (ARA) request:
#
# Hi Jonathan, I need to get the AQI-NowCast graphs of the data from the
# BAM/EBAMs that were deployed in Yosemite during the Creek fire.  Lee Tarnay
# usually helps with this but he suggested that I contact you since we are having
# trouble finding a site ID for one of the units.  What I'm looking for is the
# graphs for 4 units from 9/1/2020 - 11/15/2020 for Yosemite Village (ID 030431001),
# Wawona (ID APCD1020), El Portal (ID APCD1011) and Tuolumne Meadows (where we had
# to swap out units during that period so need to stitch them together)  We had
# an EBAM there (WRCC but unable to get the ID since it will not turn on now) from
# 8/21 -9/29/20 and then we put another EBAM (ID APCD1011) there 9/23-24/20 and
# again from 9/29-10/29/20.  Is it possible to do that?  Lee said that there is
# a way to find the site ID since you have a location with the APCD 1019.
#

library(PWFSLSmoke)

CA_monitor <-
  monitor_loadAnnual(2020) %>%
  monitor_subset(stateCodes = c("CA")) %>%
  monitor_subset(tlim = c(20200901, 20201115)) %>%
  monitor_trim()

# monitor_leaflet(CA_monitor)

YosemiteVillage <-
  ws_monitor %>%
  monitor_subset(monitorID = "060431001_01")

ws_monitor <-
  CA_monitor %>%
  monitor_subsetByDistance(
    longitude = YosemiteVillage$meta$longitude,
    latitude = YosemiteVillage$meta$latitude,
    radius = 50
  )

