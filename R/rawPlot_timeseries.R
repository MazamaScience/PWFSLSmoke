# ===== PRELIM STUFF ==========================

# TODO: Set up "add=TRUE" -- was unable to get this to work in first pass
# TODO: Shade based on wind speed and/or direction?

# IDEAS
# Move around title, labels, etc.
# change colors? e.g. AQI colors if parameter == "pm25"

# NEEDED TO RUN THIS CODE:
# MazamaSpatialUtils
# lubridate

# ===== ACTUAL STUFF ==========================

###############################################################################
# 
# This function creates a plot of raw monitoring data, with various bells and 
# whistles per user input.
#
###############################################################################
# 
# #' @keywords raw_enhance
# #' @export
# #' @title Plot Raw Monitoring Data
# #' @param df raw_enhance monitor object, as created by raw_enhance.R
# #' @param parameter OPTIONAL parameter to plot; defaults to "pm25". Other options include
# #' c("temperature","humidity","windSpeed","windDir", pressure), or any of the other raw
# #' parameters (do "names(df)" to see list of options)
# #' @param useGMT Plot w/ time axis in GMT, as opposed to local time. This is default if >1 time zone in data.
# #' @param shadedNight Shade background based on approximate sunrise/sunset times. Unavailable if >1 time zone in data.
# #' Also note that for multiple deployments, this defaults to use the lat/lon for the first deployment, which in theory
# #' could be somewhat unrepresentative, such as if deployments have a large range in latitude.
# #' @param shadedBackground Add vertical lines corresponding to wind speed, binned into quartiles. Future iterations
# #' may include options to choose which parameter to plot, which color to use, which intervals, etc.
# #' @param add (not currently supported) Option to allow overplotting on existing plot landscape
# #' @param tlim A vector that subsets the raw dataframe by time limits, e.g. c(20160915,20161010)
# #' @param type Line type; defaults to best option for each type, unless specified otherwise in this argument.
# #' @description Creates a plot of raw monitoring data as generated using raw_enhance.R.
# 
# # NOTE:  This next section has to be commented out when you build the package
# # NOTE:  but it's great to keep around for debugging
# 
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
# rawPlot_timeseries <- function(df,
#                                parameter="pm25",
#                                useGMT=FALSE,
#                                shadedNight=FALSE,
#                                shadedBackground=FALSE,
#                                #add=FALSE,
#                                tlim=NULL,
#                                type=NULL,
#                                ...) {
#   
#   # If passed as argument, check that parameter exists in names of df
#   if (!(parameter %in% names(df))) {
#     stop(paste0("'",parameter,"' does not exist in names(",deparse(substitute(df)),")",sep=""))
#   }
# 
#   # ----- Style ---------------------------------------------------------------
# 
#   # Time axis labels
#   xlabLocal <- "Date and Time (local)" # used if GMT==FALSE
#   xlabGMT <- "Date and Time (GMT)" # used if GMT==TRUE, or if time data is in >1 timezone
#   
#   # Line type default (can be overwritten)
#   if(is.null(type)) {type <- "l"; typeSpec <- FALSE} else {typeSpec <- TRUE}
# 
#   # Parameter-specific styling
#   if (parameter == "temperature") {
#     ylab <- "Air Temperature (Deg C)"
#     title <- "Air Temperature"
#   } else if (parameter == "humidity") {
#     ylab <- "Relative Humidity (%)"
#     title <- "Relative Humidity"
#   } else if (parameter == "windSpeed") {
#     ylab <- "Wind Speed (m/s)"
#     title <- "Wind Speed"
#   } else if (parameter == "windDir") {
#     ylab <- "Wind Direction (degrees)"
#     if (!typeSpec) {type <- "p"} # change from line graph to dots unless specifically requested to plot as line
#     title <- "Wind Direction"
#   } else if (parameter == "pm25") {
#     ylab <- "PM2.5 (ug/m3)"
#     title <- "PM2.5 Concentration"
#   } else if (parameter == "pressure") {
#     ylab <- "Barometric Pressure (hPa)"
#     title <- "Atmospheric Pressure"
#   } else {
#     ylab <- parameter
#     title <- parameter
#   }
# 
#   # ----- Data Preparation ----------------------------------------------------
#   
#   # Default to use GMT if >1 timezone in data
#   if (length(unique(df$timezone))>1) {
#     print("More than one time zone, so forced to plot using GMT")
#     useGMT <- TRUE
#   }
#   
#   # Set time axis data and labels
#   if (useGMT) {
#     datetime <- df$datetime
#     xlab <- xlabGMT
#   } else {
#     datetime <- lubridate::with_tz(df$datetime, tzone=df$timezone[1])
#     xlab <- xlabLocal
#   }
# 
#   # Time limit application
#   if (!is.null(tlim)) {
#     # TODO: add logic to check for tlim format
#     # TODO: warn if tlim is outside range of datetime data
#     timeMask <- datetime >= lubridate::ymd(tlim[1]) & datetime < lubridate::ymd(tlim[2])+lubridate::days(1)
#     if (sum(timeMask)==0) {
#       stop("No data contained within specified time limits, please try again.")
#     }
#     datetime <- datetime[timeMask]
#     df <- df[timeMask,]
#   }
#   
#   # Prep the data to plot, based on parameter selection by user (default = "pm25")
#   param <- df[[parameter]]
#   
#   # ----- Plotting ------------------------------------------------------------
#   
#   # Create the plot
#   plot(datetime,param,
#        xlab=xlab,
#        ylab=ylab,
#        type=type,
#        ...)
# 
#   # Shaded Night: based on first deployment lat/lon if >1 deployment; breaks if >1 time zone
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
#   # Shaded Background
#   # TODO: add shadedBackground in the same manner as above
#   if (shadedBackground) {
#     addShadedBackground(param=df$windSpeed, timeAxis=datetime, ...)
#   }
#   
#   # Add chart title
#   title(title)
#   
# }
# 
# 
# sample code for wind direction plot
# double axes from https://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
# 
# windDir <- df$windDir
# 
# cols <- rep("black",length(windDir))
# eMask <- windDir <180 & windDir>=0
# wMask <- windDir <360 & windDir>=180
# 
# cols[which(eMask)] <- "blue"
# cols[which(wMask)] <- "red"
# 
# #par(mar=c(5, 12, 4, 4) + 0.1)
# 
# windDir[wMask] <- 360-windDir[wMask]
# 
# plot(df$datetime,windDir,col=cols,ylim=c(180,0))
# title("Wind Direction: Red = from east, Blue = from West") #this really needs a second axis to be legit
# #axis(4,ylim=c(0,20),col="red",lwd=2)
# 
# #basic plot
# plot(df$datetime,df$windDir,col=cols,ylim=c(180,0))
# #box()
# #axis(2, ylim=c(0,180),col="blue",lwd=2)
# #axis(1)