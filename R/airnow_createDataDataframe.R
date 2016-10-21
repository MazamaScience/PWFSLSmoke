#' @keywords AirNow
#' @export
#' @title Return Reshaped, Monthly Dataframes of AirNow Data
#' @param user user name
#' @param pass pass
#' @param parameter name of desired pollutant or NULL for all pollutants
#' @param yearMonth desired year and month  (integer or character representing YYYYMM)
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical requesting verbose output from libcurl
#' @description The airnow_createDataDataframe() function uses the \link{airnow_downloadData} function 
#' to download monthly dataframes of AirNow data and restructures that data into a format that is compatible
#' with the PWFSLSmoke package \emph{ws_monitor} data model.
#' 
#' 
#' Parameters included in AirNow data include at least the following list:
#' \enumerate{
#' \item{BARPR}
#' \item{BC}
#' \item{CO}
#' \item{NO}
#' \item{NO2}
#' \item{NO2Y}
#' \item{NO2X}
#' \item{NOX}
#' \item{NOOY}
#' \item{OC}
#' \item{OZONE}
#' \item{PM10}
#' \item{PM2.5}
#' \item{PRECIP}
#' \item{RHUM}
#' \item{SO2}
#' \item{SRAD}
#' \item{TEMP}
#' \item{UV-AETH}
#' \item{WD}
#' \item{WS}
#' }
#' 
#' Setting \code{parameter=NULL} will generate a dataframe with all parameters.
#' @return Returns a list of data frames where each dataframe is contains all data for a unique parameter (e.g: PM2.5, NOX)
#' @seealso \link{airnow_downloadData}
#' @examples
#' \dontrun{
#' AirnowData <- airnow_createDataDataframe(user, pass, "PM2.5", 201507)
#' }

airnow_createDataDataframe <- function(user, pass, parameter="PM2.5", yearMonth, tries=6, verbose=FALSE) {
  
  # ----- Data Download -------------------------------------------------------

  logger.info('Downloading AirNow data for %s',yearMonth)
  
  # Calculate the number of hours in the month of the interest
  startdate <- paste0(yearMonth, '0100')
  starttime <- lubridate::ymd_h(startdate)
  hours <- 24 * as.numeric(lubridate::days_in_month(starttime))
  
  # Create the data frame that holds a month worth of AirNow data
  airnowRaw <- airnow_downloadData(user, pass, parameter=parameter, startdate=startdate, hours=hours, tries=tries, verbose=verbose)
  
  # ----- Data Reshaping ------------------------------------------------------

  logger.debug('Reshaping AirNow data for %s...',yearMonth)

  # NOTE:  Example lines from the aggregated dataframe:
  # NOTE:
  # NOTE:    ValidDate ValidTime     AQSID   SiteName GMTOffset ParameterName ReportingUnits Value                  DataSource
  # NOTE:  1  08/01/14     00:00 000010102 St. John's        -4         OZONE            PPB    12 Newfoundland & Labrador DEC
  # NOTE:  2  08/01/14     00:00 000020301 WELLINGTON        -4           NO2            PPB     0          Environment Canada
  # NOTE:  3  08/01/14     00:00 000020301 WELLINGTON        -4         OZONE            PPB    18          Environment Canada
  # NOTE:  4  08/01/14     00:00 000020301 WELLINGTON        -4         PM2.5          UG/M3     7          Environment Canada
  # NOTE:  5  08/01/14     00:00 000020301 WELLINGTON        -4            NO            PPB     0          Environment Canada

  # Get a list of parameters
  parameters <- sort(unique(airnowRaw$ParameterName))

  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()

  # Use dplyr and reshape2 packages to seprate the data by the types and restructure each data frame
  # TODO: creating static timeaxis for each parameter for the sanity check
  for (parameter in parameters) {

    # Create datetime variable
    df <- dplyr::filter(airnowRaw, airnowRaw$ParameterName == parameter)
    datestamp <- paste0(df$ValidDate, ' ', df$ValidTime)
    df$datetime <- lubridate::mdy_hm(datestamp) # 'mdy_hm', not 'ymd_hm'
    # Guarantee unique rows
    df <- dplyr::distinct(df)
    # Melt and recast
    melted <- reshape2::melt(df, id.vars=c('datetime','AQSID'), measure.vars=c('Value'))
    dfList[[parameter]] <- reshape2::dcast(melted, datetime ~ AQSID)

  }

  # NOTE:  Some parameters, especially those with few monitors, may not have measurements for
  # NOTE:  for every single hour. Here we guarantee that the reshaped dataframes we return will
  # NOTE:  have a row for every single hour in a month, even if that row is filled with NAs.

  # Guarantee that all times are found by merging a uniform time axis dataframe with AirNow dataframes
  timeAxis <- seq(starttime, starttime + lubridate::dhours(hours), by='hours')
  timeDF <- data.frame(datetime=timeAxis)
  for (parameter in parameters) {
    # Join data to uniform time axis
    dfList[[parameter]] <- suppressMessages( dplyr::full_join(timeDF, dfList[[parameter]]) )
    # Apply QC with different valid limits for each parameter
    if (parameter == 'PM2.5') {
      airnow_qualityControl(dfList[[parameter]], limits=c(-Inf,Inf))#limits=c(-10,500.4))
    } else if (parameter == 'WS') {
      airnow_qualityControl(dfList[[parameter]], limits=c(-5,142.2))
    } else if (parameter == 'WD') {
      airnow_qualityControl(dfList[[parameter]], limits=c(0,360))
    }
    # TODO:  Add more QC limits
    
  }


#   # ----- Post-Processing: Quality Control ------------------------------------
#   
#   logger.debug('Applying QC to AirNow data for %s...',yearMonth)
#   
#   # Make an empty list for Quality Control Report
#   QCInfoList <- list()
#   
#   # TODO:  Add notes describing where do the QC ranges for each parameter come from?
#   
#   # Define the low and high thresholds for the each parameter
#   loLimit <- list(OZONE=-10, WS=-5, 
#                   NO2=-10, SRAD=-10,
#                   NO=-10, TEMP=-65,
#                   PM2.5=-10, PRECIP=0,
#                   PM10=-10, RHUM=0,
#                   NOX=-10, WD=0,
#                   CO=-10, BARPR=870,
#                   SO2=-10, 'UV-AETH'=0,
#                   BC=-10,
#                   NOY=-10,
#                   NO2Y=-10,
#                   OC=-10)
# 
#   hiLimit <- list(OZONE=374, WS=142.2, 
#                   NO2=2049, SRAD=1413,
#                   NO=2049, TEMP=60,
#                   PM2.5=500.4, PRECIP=350,
#                   PM10=604, RHUM=135,
#                   NOX=2049, WD=360,
#                   CO=50.4, BARPR=1083.3,
#                   SO2=1004, 'UV-AETH'=14, 
#                   BC=50,
#                   NOY=2049,
#                   NO2Y=2049,
#                   OC=50)
#   

  return(dfList)

}
  
