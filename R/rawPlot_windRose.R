#' @keywords raw_enhance
#' @export
#' @title Create a Wind Rose Plot from raw_enhance Object
#' @param df raw_enhance monitor object, as created by the raw_enhance() function
#' @param tlim A vector that subsets the raw dataframe by time limits, e.g. c(20160915,20161010)
#' @param localTime use local times; defaults to GMT if >1 time zone in data.
#' @param ... additional arguments to pass on to openair::windRose()
#' @description Create wind rose plot from raw_enhance object. Based on openair::windRose().
#' @examples
#' \dontrun{
#' df <- PWFSLSmoke::airsis_createRawDataframe(startdate = 20160901,enddate=20161015, unitID = 1012)
#' df <- raw_enhance(df,rawSource='AIRSIS')
#' rawPlot_windRose(df)
#' }

rawPlot_windRose <- function(df,
                             tlim=NULL,
                             localTime=TRUE,
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
  if ( !('angle' %in% names(argsList)) ) {
    argsList$angle <- 15
  }
  if ( !('ws' %in% names(argsList)) ) {
    argsList$ws <- "windSpeed"
  }
  if ( !('wd' %in% names(argsList)) ) {
    argsList$wd <- "windDir"
  }
  
  # ----- Plotting -------------------------
  
  # Create Plot
  do.call(openair::windRose,argsList)
  
}
