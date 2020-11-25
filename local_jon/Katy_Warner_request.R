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
library(PWFSLSmokePlots)

# Get all California monitors for the time period
CA_monitor <-
  monitor_loadAnnual(2020) %>%
  monitor_subset(stateCodes = c("CA")) %>%
  monitor_subset(tlim = c(20200901, 20201115)) %>%
  monitor_trim()

monitor_leaflet(CA_monitor)

# Isolate the Yosemite Village monitor
Yosemite_Village <-
  CA_monitor %>%
  monitor_subset(monitorID = "060431001_01")

# Find all monitors within 20 km of Yosemite Village
ws_monitor <-
  CA_monitor %>%
  monitor_subsetByDistance(
    longitude = Yosemite_Village$meta$longitude,
    latitude = Yosemite_Village$meta$latitude,
    radius = 30
  )

monitor_leaflet(ws_monitor)

# Create daily-hourly barplot
dailyHourlyBarplot(ws_monitor)

# ==============================================================================

# So, what I really need for this report are the ones labeled Yosemite Village -
# Visitor Center, El Portal - 5518 Foresta Rd, Yosemite National Park -7799
# Chilnualna Rd, and the stitched together ones of Long Barn - 9075 Tuolumne
# Meadows Lodge Rd with Long Barn - 9073 Tuolumne Meadows Lodge Rd.  And it would
# be good to put the Groveland - Hodgdon Meadow Access Rd at the bottom, since
# it clearly showed some impacts (but wasn't on the outlook.)

El_Portal <-
  CA_monitor %>%
  monitor_subset(monitorID = "lon_.119.784_lat_37.675_apcd.1011")

Chilnualna_Rd <-
  CA_monitor %>%
  monitor_subset(monitorID = "lon_.119.658_lat_37.540_apcd.1020")

Groveland <-
  CA_monitor %>%
  monitor_subset(monitorID = "lon_.119.860_lat_37.797_apcd.1019")

# Combine the two Long Barn timeseries
Long_Barn_siteName <- "Long Barn - Tuolomne Meadows Lodge Rd"

Long_Barn_9705 <-
  CA_monitor %>%
  monitor_subset(monitorID = "lon_.119.343_lat_37.877_wrcc.smy1")
Long_Barn_9705$meta$siteName <- Long_Barn_siteName
Long_Barn_9705$meta$monitorID <- "LongBarnFakeMonitorID"
names(Long_Barn_9705$data) <- c("datetime", "LongBarnFakeMonitorID")

Long_Barn_9703 <-
  CA_monitor %>%
  monitor_subset(monitorID = "lon_.119.343_lat_37.877_apcd.1019")
Long_Barn_9703$meta$siteName <- Long_Barn_siteName
Long_Barn_9703$meta$monitorID <- "LongBarnFakeMonitorID"
names(Long_Barn_9703$data) <- c("datetime", "LongBarnFakeMonitorID")

# Join the two Long Barn timeseries
Tuolomne_Meadows <- monitor_join(Long_Barn_9705, Long_Barn_9703)


# Combine monitors of interest
my_monitors <- monitor_combine(list(
  Yosemite_Village, El_Portal, Chilnualna_Rd,
  Tuolomne_Meadows, Groveland
))

# New plot
dailyHourlyBarplot(my_monitors)
