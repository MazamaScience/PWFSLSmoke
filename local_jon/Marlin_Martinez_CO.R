# Marlin Martinez work with CO data for 2020
#
# Using manually created data files which have been uploaded to haze.airfire.org
#
# From:  https://en.wikipedia.org/wiki/Air_quality_index
#
#       AQI Category, Pollutants and Health Breakpoints
# AQI Category          (Range)	PM10 (24hr)	PM2.5 (24hr)	NO2 (24hr)	O3 (8hr)	CO (8hr)	SO2 (24hr)	NH3 (24hr)	Pb (24hr)
# Good                   (0–50)	       0–50	        0–30	      0–40	    0–50	   0–1.0	      0–40	     0–200	    0–0.5
# Satisfactory         (51–100)	     51–100	       31–60	     41–80	  51–100	 1.1–2.0	     41–80	   201–400	  0.5–1.0
# Moderately polluted (101–200)	    101–250	       61–90	    81–180	 101–168	  2.1–10	    81–380	   401–800	  1.1–2.0
# Poor                (201–300)	    251–350	      91–120	   181–280	 169–208	   10–17	   381–800	  801–1200  	2.1–3.0
# Very poor           (301–400)	    351–430	     121–250	   281–400	 209–748	   17–34	  801–1600   1200–1800  	3.1–3.5
# Severe              (401–500)	       430+	        250+       	400+	    748+	     34+	     1600+     	 1800+      	3.5+
#

library(PWFSLSmoke)

library(dplyr)
library(ggplot2)

# ----- Load CO data -----------------------------------------------------------

# NOTE:  ~load() functions don't work properly with `parameter = "CO"` so we do
# NOTE:  it manually.
PM25_url <- "https://haze.airfire.org/monitoring/AirNow/RData/2020/airnow_PM2.5_2020.RData"
CO_url   <- "https://haze.airfire.org/monitoring/AirNow/RData/2020/airnow_CO_2020.RData"

CO_2020 <- get(load(url(CO_url)))

# ----- Leaflet map ------------------------------------------------------------

# US CO levels
CO_breaks_8 <- c(-Inf, 1, 2, 10, 17, 34, Inf)

# TODO:  Specifying custom breaks to monitor_leaflet() doesn't work.
monitor_leaflet(
  ws_monitor = CO_2020,
  slice = get("max"),
  breaks = AQI$breaks_24,
  colors = AQI$colors,
  labels = AQI$names,
  legendTitle = "Max AQI Level",
  radius = 10,
  opacity = 0.7,
  maptype = "terrain",
  popupInfo = c("siteName", "monitorID", "elevation")
)

# NOTE:  Two monitors in Portland are extremely high.

# NOTE:  A timeseries plot of this data is not trustworthy
Portland_SE_Lafayette <- monitor_subset(CO_2020, monitorIDs = "410510080_01")
monitor_timeseriesPlot(Portland_SE_Lafayette)

# ----- CA all data plot -------------------------------------------------------

CA <-
  monitor_subset(
    CO_2020,
    stateCodes = "CA",
    tlim = c(20200430, 20201102)
  )

monitor_timeseriesPlot(CA, style = 'gnats', ylab = "CO", main = "California hourly CO values")

# ----- Guts of addAQI~() functions -----

# addAQILines()
graphics::abline(h = CO_breaks_8, col = AQI$colors)

# addAQIStackedBar()
width = .01
height = 1
usr <- par("usr")
l <- usr[1]
r <- usr[1] + width*(usr[2] - usr[1])

for (i in 1:6) {
  rect(l,
       min(max(0, CO_breaks_8[i]), height*usr[4]),
       r,
       min(AQI$breaks_24[i+1], height*usr[4]),
       col = AQI$colors[i],
       xpd = NA,
       border = NA
  )
}

# addAQILegend()
legend(
  x = "topright",
  y = NULL,
  col = rev(AQI$colors),
  legend = rev(AQI$names),
  pch = 16,
  title = "Air Quality Index"
)

# ----- Chico daily average ----------------------------------------------------

Chico <- monitor_subset(CA, monitorIDs = "060070008_01")

# TODO:  monitor_dailyBarplot() does not support non-PM25 parameters
monitor_dailyBarplot(Chico, ylab = "CO", main = "2020 Daily Average CO -- California")

# Bet we can easily get a dataframe of daily means
CA_dailyMean <- monitor_dailyStatistic(CA)

# Create a "tidy" dataframe for working with ggplot2
CA_DM_tidy <-
  monitor_toTidy(CA_dailyMean) %>%
  rename(co = "pm25")

# ----- Using ggplot with CA_DM_tidy -------------------------------------------

Chico_DM_tidy <-
  CA_DM_tidy %>%
  filter(monitorID == "060070008_01")

gg <-
  ggplot(Chico_DM_tidy) +
  geom_col(aes(datetime, co)) +
  # Opportunity to add lots more ggplot code here for colors, faceting, etc.
  ggtitle("Daily Mean CO in Chico, A")

print(gg)





