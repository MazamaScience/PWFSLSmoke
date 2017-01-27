#' @keywords raw
#' @export
#' @import graphics
#' @title Create Pollution Rose Plot from a Raw Dataframe
#' @param df enhanced, raw dataframe as created by the raw_enhance() function
#' @param parameter parameter to plot
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime use local times; defaults to GMT if >1 time zone in data.
#' @param normalize normalize slices to fill entire area, allowing for easier comparison of counts of magnitudes by direction
#' @param ... additional arguments to pass on to openair::pollutionRose()
#' @description Create pollution rose plot from raw_enhance object based on openair::pollutionRose(). If normalized, black line
#' indicates frequency by direction.
#' @examples
#' \dontrun{
#' df <- PWFSLSmoke::airsis_createRawDataframe(startdate = 20160901, enddate=20161015, unitID = 1012)
#' df <- raw_enhance(df,rawSource='AIRSIS')
#' rawPlot_pollutionRose(df)
#' }

rawPlot_pollutionRose <- function(df,
                                  parameter="pm25",
                                  tlim=NULL,
                                  localTime=TRUE,
                                  normalize=FALSE,
                                  ...) {
  
  # ----- Data Preparation -----------------
  
  # Identify timezone(s)
  timezone <- unique(df$timezone)
  
  # Force timezone to UTC if >1 timezone in metadata for monitorIDs
  if ( length(timezone)>1 ) { # note that we will only enter this condition if localTime==TRUE
    if ( localTime ) {
      warning(">1 timezone in data: Timezone (including tlim, if specified) forced to UTC")
      timezone <- "UTC"
    }
  }
  
  # Set timezone to UTC if localTime==FALSE
  if ( !localTime ) {
    timezone <- "UTC"
  }
  
  # Set time axis data
  df$datetime <- lubridate::with_tz(df$datetime, tzone=timezone)
  
  # Time limit application
  # TODO: add logic to check for tlim format
  # TODO: warn if tlim is outside range of datetime data
  if ( !is.null(tlim) ) {
    # When tlim is specified in whole days we add hours to get the requsted full days
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
    tlim <- PWFSLSmoke::parseDatetime(tlim, timezone=timezone)
    
    # Create time mask and subset data
    timeMask <- df$datetime >= tlim[1] & df$datetime <= tlim[2]
    if ( sum(timeMask)==0 ) {
      stop("No data contained within specified time limits, please try again.")
    }
    df <- df[timeMask,]
  }
  
  # Populate arguments for openair::windRose
  argsList <- list(...)
  argsList$mydata <- df
  argsList$pollutant <- parameter
  argsList$normalise <- normalize
  if ( !('angle' %in% names(argsList)) ) {
    argsList$angle <- 45
  }
  if ( !('ws' %in% names(argsList)) ) {
    argsList$ws <- "windSpeed"
  }
  if ( !('wd' %in% names(argsList)) ) {
    argsList$wd <- "windDir"
  }
  
  # ----- Plotting -------------------------
  
  # Create Plot
  do.call(openair::pollutionRose,argsList)
  
}

# NOTE: As an interesting check on the binning by wind direction, run the function above with parameter="windDir". It
# NOTE: is interesting to note that the boundary bins do not fully capture; so the direction bounds must be offset!
