# # From monitor_timeseriesPlot(): Creates a time series plot of PM2.5 data from a
# # ws_monitor object (see note below). Optional arguments color code by AQI index,
# # add shading to indicate nighttime, and adjust the time display (GMT vs. local).
# 
# # ======
# 
# # Create a function that accepts an enhanced raw dataframe and creates a time series plot with the same
# # features as monitor_timeseriesPlot().
# 
# # NOTE: this is a placeholder for now.  Basic concept is captured, and will develop further at a later date.
# 
# # IDEAS
# # Subset data by time?
# # Move around title, labels, etc.
# # change colors?
# 
# # use the following to set up some dummy raw_enhanced data to play with
# if (FALSE) {
# 
#   library(PWFSLSmoke)
#   library(openair)
#   setwd("~/Projects/PWFSLSmoke/")
# 
#   load("localData/airsis_rawList.RData")
#   source('~/Projects/Mazama/PWFSLSmoke/R/raw_enhance.R', echo=FALSE)
# 
#   raw <-   raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
#   ebam_airsis <- raw_enhance(raw, rawSource = rawSource)
# 
#   raw <- airsis_rawList$Naches; rawSource <- "AIRSIS" #ESAM AIRSIS
#   esam_airsis <- raw_enhance(raw, rawSource = rawSource)
# 
#   raw <- airsis_rawList$Usk; rawSource <- "WRCC" #ESAM WRCC
#   esam_wrcc <- raw_enhance(raw, rawSource = rawSource)
# 
#   rm(raw)
#   rm(rawSource)
# 
# }
# 
# rawPlot_timeseries <- function(raw) {
# 
# 
# }
# 
# 
# 
# # FROM DNR_UTILS.R
# 
# 
# DNR_stoveWindPlot <- function(ws_monitor, raw,
#                               tempVar='AT', humidityVar='RHx', windVar='W.S',
#                               title='', tlim=NULL) {
# 
#   # Flusing Winds
#   windBin <- .bincode(raw$W.S, c(-Inf,1,2,5,10,Inf))
#   for (i in 5) { colors[i] <- adjustcolor('blue',(i-1)/5) }
# 
#   # Create fancy woodburning index
#   stoveIndex <- woodStoveIndex(raw, tempVar, humidityVar, rollingWidth=12, rollingAlign='left')
#   stoveSmoke <- ws_monitor$data[,2]
#   stoveSmoke[stoveIndex < 0.5] <- NA
# 
#   monitor_timeseriesPlot(ws_monitor, type='l', shadedNight=FALSE)
#   points(stoveSmoke ~ ws_monitor$data$datetime, pch=16, col='purple')
#   abline(v=raw$datetime, col=colors[windBin])
#   legend('topleft', legend=c("Woodstove Index", "0-1 m/s Wind", "1-2 m/s Wind", "2-5 m/s Wind"), fill=c('purple',colors[1],colors[2],colors[3]))
# 
# }