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
# # 
# # Need the following to run:
# # MazamaSpatialUtils
# # lubridate
# #
# 
# # ACTUAL CODE ==================
# 
# # use the following to set up some dummy raw_enhanced data to play with.  Comment out before adding to package.
# if (FALSE) {
# 
#   library(PWFSLSmoke)
#   library(openair)
#   setwd("~/Projects/PWFSLSmoke/")
# 
#   load("localData/airsis_rawList.RData")
#   source('~/Projects/PWFSLSmoke/R/raw_enhance.R', echo=FALSE)
# 
#   raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
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
# # and here's the actual function
# 
# rawPlot_timeseries <- function(df, parameter="pm25", useGMT=FALSE, shadedNight=FALSE, add=FALSE, ...) {
# 
#   args <- list(...)
#   type <- args[['type']]
# 
#   print(paste("type passed in via dots:",type))
# 
#   if(is.null(type)) {type <- "l"; typeSpec <- FALSE} else {typeSpec <- TRUE}
#   
#   # General Formatting
#   xlab <- "Date and Time (local)" #overwritten to say "GMT" at end if useGMT is true -- see time section below
# 
#   #TODO: enable AQI colors if parameter == "pm25"
#   #TODO: include warning if AQI style selected but parameter != "pm25"
# 
#   # Parameter-specific formatting
#   if (parameter == "temperature") {
#     ylab <- "Air Temperature (Deg C)"
#   } else if (parameter == "humidity") {
#     ylab <- "Relative Humidity (%)"
#   } else if (parameter == "windSpeed") {
#     ylab <- "Wind Speed (m/s)"
#   } else if (parameter == "windDir") {
#     ylab <- "Wind Direction (degrees)"
#     if (!typeSpec) {type <- "p"} # change from line graph to dots unless specifically requested to plot as line
#   } else if (parameter == "pm25") {
#     ylab <- "PM2.5 (ug/m3)"
#   } else if (parameter == "pressure") {
#     ylab <- "Barometric Pressure (hPa)"
#   }
# 
#   # MISC
#   lon <- df$longitude[1]
#   lat <- df$latitude[1]
# 
#   # TIME ===========
# 
#   # NOTE: Moved timezome assignment to the raw_enhance function.
#   # NOTE: May still need to check for monitors that have >1 deployment -- see deploymentID field
#   
#   if (useGMT) {
#     datetime <- df[["datetime"]]
#     xlab <- "Date and Time (GMT)"
#   } else {
#     timezone <- MazamaSpatialUtils::getTimezone(lon, lat, countryCodes=NULL, useBuffering=TRUE) #modified from addMazamaMetadata.R
#     datetime <- lubridate::with_tz(df$datetime, tzone=timezone)
#   }
#   
#   # PLOT ============
#   
#   print(paste("Before plotting, type =",type))
#   
#   # # need to merge type back into the ... argument for plot function...
#   args[['type']] <- type
#   
#   print(paste("args:",args))
#   #do.call(identical, args)
#   
#   param <- df[[parameter]] #used in Plot function below for main item to be plotted
# 
#   plot(datetime,param,
#        xlab=xlab,
#        ylab=ylab,
#        ...=args)
# 
#   # shade nighttime hours
#   ###  if(nrow(meta) == 1 & !useGMT & shadedNight) {
#   if ( shadedNight && !useGMT ) {
#     timeInfo <- PWFSLSmoke::timeInfo(datetime, lon, lat, timezone)
#     PWFSLSmoke::addShadedNights(timeInfo)
#   }
# 
#   #TODO: add shadedBackground in the same manner as above
# 
# }
# 
# 
# 
