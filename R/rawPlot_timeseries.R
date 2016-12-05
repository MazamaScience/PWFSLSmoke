# # NOTE: this is a placeholder for now.  Basic concept is captured, and will develop further at a later date.
# 
# # TASK ================
# # Create a function that accepts an enhanced raw dataframe and creates a time series plot with the same
# # features as monitor_timeseriesPlot().
# 
# # OVERVIEW ===============
# 
# # Let's include functionality to do the following:
# # Take raw_enhance file as first argument
# # Get told what parameter to plot
# # Take in similar arguments to monitor_timeseriesPlot (i.e. AQIStyle='', useGMT=FALSE(?), shadedNight=FALSE(?), add=FALSE)
# # Take in any additional arguments to the plot command as needed
# # Other argument may be to shade based on wind speed and/or direction?
# 
# # IDEAS
# # Subset data by time?
# # Move around title, labels, etc.
# # change colors?
# 
# # OUTLINE ===============
# 
# # function call takes in raw_enhance object (df), plus additional argument (as described below/above)
# # 
# # create plot of selected parameters
# 
# # ACTUAL CODE ==================
# 
# # use the following to set up some dummy raw_enhanced data to play with
# if (FALSE) {
#   
#   library(PWFSLSmoke)
#   library(openair)
#   setwd("~/Projects/PWFSLSmoke/")
#   
#   load("localData/airsis_rawList.RData")
#   source('~/Projects/PWFSLSmoke/R/raw_enhance.R', echo=FALSE)
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
# 
# # and here's the actual function
# 
# rawPlot_timeseries <- function(df, parameter="pm25", AQIStyle='', add=FALSE, ...) {
#   
#   param <- df[[parameter]]
#   
#   plot(rawE$datetime,param,
#        xlab="Date and Time",
#        ylab=parameter)
#   
#   
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