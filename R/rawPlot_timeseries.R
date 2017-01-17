# ===== PRELIM STUFF ==========================

# TODO: Set up "add=TRUE" -- was unable to get this to work in first pass
# TODO: update time zone acquisition to eliminate warning for OlsonNames
# TODO: prohibit user from using both shadedNights and shadedBackground?
# TODO: Set up to use do.call(arg, argList) to enable easier passing of arguments to sub-functions

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

#' @keywords raw_enhance
#' @export
#' @title Plot Raw Monitoring Data
#' @param df raw_enhance monitor object, as created by the raw_enhance() function
#' @param parameter raw parameter to plot. Default = "pm25". Other options include
#' c("temperature","humidity","windSpeed","windDir", "pressure"), or any of the other raw
#' parameters (do "names(df)" to see list of options)
#' @param localTime use local times; defaults to GMT if >1 time zone in data.
#' @param tlim A vector that subsets the raw dataframe by time limits, e.g. c(20160915,20161010)
#' @param shadedNight Shade background based on approximate sunrise/sunset times. Unavailable if >1 time zone in data.
#' Also note that for multiple deployments, this defaults to use the lat/lon for the first deployment, which in theory
#' could be somewhat unrepresentative, such as if deployments have a large range in latitude.
#' @param shadedBackground Add vertical lines corresponding to a second parameter; currently defaults to wind speed binned into quartiles. Future iterations
#' may include options to choose which parameter to plot, which color to use, which intervals, etc.
#' @param type Line type; defaults to best option for each type, unless specified otherwise in this argument. To replace w/ ...
# #' @param lineLwd line width for timeseries line; to replace w/ do.call and argsList...
#' @param sbLwd shaded background line width
#' @param ... additional arguments to pass to lines() function
#' @description Creates a plot of raw monitoring data as generated using raw_enhance().

# TODO: Update from useGMT to localTime, and switch logic...

# NOTE:  This next section has to be commented out when you build the package
# NOTE:  but it's great to keep around for debugging
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
#   rawEnhanceList <- list()
# 
#   raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
#   rawEnhanceList$EBAM_AIRSIS <- raw_enhance(raw, rawSource = rawSource)
# 
#   raw <- airsis_rawList$Naches; rawSource <- "AIRSIS" #ESAM AIRSIS
#   rawEnhanceList$ESAM_AIRSIS <- raw_enhance(raw, rawSource = rawSource)
# 
#   raw <- airsis_rawList$Usk; rawSource <- "WRCC" #ESAM WRCC
#   rawEnhanceList$ESAM_WRCC <- raw_enhance(raw, rawSource = rawSource)
# 
#   rm(raw)
#   rm(rawSource)
#   rm(airsis_rawList)
# 
#   lapply(rawEnhanceList,head)
# 
# }

rawPlot_timeseries <- function(df,
                               parameter="pm25",
                               localTime=TRUE,
                               tlim=NULL,
                               shadedNight=TRUE,
                               shadedBackground='windSpeed', #specify parameter to shade
                               type=NULL,
                               sbLwd=1,
                               #add=FALSE,
                               # linelwd=1, to replace using argsList logic
                               ...) {

  # ----- Initial coherency checks -------------
  
  # Verify plot parameter exists
  if (!(parameter %in% names(df))) {
    stop(paste0("'",parameter,"' does not exist in names(",deparse(substitute(df)),")",sep=""))
  }
  
  # Verify shadedBackground parameter exists
  if ( !(is.null(shadedBackground)) ) {
    if ( !(shadedBackground %in% names(df)) ) {
      stop(paste0("Shaded background parameter '",parameter,"' does not exist in names(",deparse(substitute(df)),")",sep=""))
    }
  }

  # ----- Style ---------------------------------------------------------------

  # Time axis labels
  xlabLocal <- "Date and Time (local)" # used if GMT==FALSE
  xlabGMT <- "Date and Time (GMT)" # used if GMT==TRUE, or if time data is in >1 timezone

  # Line type default (can be overwritten) -- to overhaul using do.call and argsList
  if(is.null(type)) {type <- "l"; typeSpec <- FALSE} else {typeSpec <- TRUE}

  # Parameter-specific styling
  if (parameter == "temperature") {
    ylab <- "Air Temperature (Deg C)"
    title <- "Air Temperature"
  } else if (parameter == "humidity") {
    ylab <- "Relative Humidity (%)"
    title <- "Relative Humidity"
  } else if (parameter == "windSpeed") {
    ylab <- "Wind Speed (m/s)"
    title <- "Wind Speed"
  } else if (parameter == "windDir") {
    ylab <- "Wind Direction (degrees)"
    if (!typeSpec) {type <- "p"} # change from line graph to dots unless specifically requested to plot as line
    title <- "Wind Direction"
  } else if (parameter == "pm25") {
    ylab <- expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")"))
    title <- expression("PM"[2.5]*" Concentration")
  } else if (parameter == "pressure") {
    ylab <- "Barometric Pressure (hPa)"
    title <- "Atmospheric Pressure"
  } else {
    ylab <- parameter
    title <- parameter
  }

  # ----- Data Preparation ----------------------------------------------------

  # Default to use GMT if >1 timezone in data
  if (length(unique(df$timezone))>1) {
    print("More than one time zone, so forced to plot using GMT")
    localTime <- FALSE
  }

  # Set time axis data and labels
  if ( localTime ) {
    datetime <- lubridate::with_tz(df$datetime, tzone=df$timezone[1])
    xlab <- xlabLocal
  } else {
    datetime <- df$datetime
    xlab <- xlabGMT
  }

  # Time limit application
  if ( !is.null(tlim) ) {
    # TODO: add logic to check for tlim format
    # TODO: warn if tlim is outside range of datetime data
    timeMask <- datetime >= lubridate::ymd(tlim[1]) & datetime < lubridate::ymd(tlim[2])+lubridate::days(1)
    if (sum(timeMask)==0) {
      stop("No data contained within specified time limits, please try again.")
    }
    datetime <- datetime[timeMask]
    df <- df[timeMask,]
  }

  # Prep the data to plot, based on parameter selection by user (default = "pm25")
  param <- df[[parameter]]

  # ----- Plotting ------------------------------------------------------------

  # Create the plot

  if ( shadedNight==TRUE || !is.null(shadedBackground) ) {
    plot(datetime,param,
         type="n",
         xlab=xlab,
         ylab=ylab)
  }

  # Shaded Night: based on first deployment lat/lon if >1 deployment; breaks if >1 time zone
  if (shadedNight) {
    if (length(unique(df$timezone))>1) {
      stop("Can't do shaded night for more than one time zone!!")
    } else {
      lon <- df$longitude[1]
      lat <- df$latitude[1]
      timezone <- df$timezone[1]
      if ( localTime ) {
        timeInfo <- PWFSLSmoke::timeInfo(datetime, lon, lat, timezone)
        PWFSLSmoke::addShadedNights(timeInfo)
      } else {
        timeInfo <- PWFSLSmoke::timeInfo(df$datetime, lon, lat, timezone)
        PWFSLSmoke::addShadedNights(timeInfo)
      }
    }
  }

  # Shaded Background
  # TODO: add shadedBackground in the same manner as above
  if (!is.null(shadedBackground)) {
    # TODO: Polish up addShadedBackground and then uncomment the line below
    #addShadedBackground(param=df[[shadedBackground]], timeAxis=datetime, lwd=sbLwd, ...)
  }

  # Create the actual data plot (on top of background shading if it exists)
  lines(datetime,param,
       #xlab=xlab,
       #ylab=ylab,
       type=type,
       lwd=1, # to replace this and other calls using do.call and argsList...
       ...)

  # Add chart title
  title(title)

}
