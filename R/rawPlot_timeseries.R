# # PRELIM STUFF ==========
# 
# # TODO: Subset data by time
# # TODO: Shade based on wind speed and/or direction?
# # TODO: Add chart titles
# 
# # IDEAS
# # Move around title, labels, etc.
# # change colors? e.g. AQI colors if parameter == "pm25"
# 
# # NEEDED TO RUN THIS CODE:
# # MazamaSpatialUtils
# # lubridate
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
# # FUNCTION ============================
# 
# rawPlot_timeseries <- function(df, parameter="pm25", useGMT=FALSE, shadedNight=FALSE, add=FALSE, tlim=NULL, type=NULL, ...) {
# 
#   # General Formatting
#   xlabLocal <- "Date and Time (local)"
#   xlabGMT <- "Date and Time (GMT)"
#   if(is.null(type)) {type <- "l"; typeSpec <- FALSE} else {typeSpec <- TRUE}
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
#   } else {
#     ylab <- parameter
#   }
# 
#   # TIME ==========
#   
#   #default to use GMT if >1 TZ
#   if (length(unique(df$timezone))>1) {
#     print("More than one time zone, so forced to plot using GMT")
#     useGMT <- TRUE
#   }
#   
#   if (useGMT) {
#     datetime <- df$datetime
#     xlab <- xlabGMT
#   } else {
#     datetime <- lubridate::with_tz(df$datetime, tzone=df$timezone[1])
#     xlab <- xlabLocal
#   }
# 
#   # PLOT ============
# 
#   param <- df[[parameter]] #used in Plot function below for main item to be plotted
# 
#   plot(datetime,param,
#        xlab=xlab,
#        ylab=ylab,
#        type=type,
#        ...)
# 
#   #SHADED NIGHT ===============
#   
#   #apply shaded night based on first deployment lat/lon if >1 deployment.  Breaks if >1 time zone.
#   if (shadedNight) {
#     if (length(unique(df$timezone))>1) {
#       stop("Can't do shaded night for more than one time zone!!")
#     } else {
#       lon <- df$longitude[1]
#       lat <- df$latitude[1]
#       timezone <- df$timezone[1]
#       if (useGMT) {
#         timeInfo <- PWFSLSmoke::timeInfo(df$datetime, lon, lat, timezone)
#         PWFSLSmoke::addShadedNights(timeInfo)
#       } else {
#         timeInfo <- PWFSLSmoke::timeInfo(datetime, lon, lat, timezone)
#         PWFSLSmoke::addShadedNights(timeInfo)
#       }
#     }
#   }
# 
#   #SHADED BACKGROUND =============
#   # TODO: add shadedBackground in the same manner as above
# 
# }

