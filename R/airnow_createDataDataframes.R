#' @keywords AirNow
#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.warn
#'
#' @title Return reshaped dataframes of AirNow data
#'
#' @param parameters Vector of names of desired pollutants or NULL for all
#' pollutants.
#' @param startdate Desired start date (integer or character representing
#' YYYYMMDD[HH]).
#' @param hours Desired number of hours of data to assemble.
#'
#' @return List of dataframes where each dataframe contains all data for a
#' unique parameter (e.g: "PM2.5", "NOX").
#'
#' @description This function uses the \link{airnow_downloadParseData} function
#' to download monthly dataframes of AirNow data and restructures that data into
#' a format that is compatible with the PWFSLSmoke package \emph{ws_monitor}
#' data model.
#'
#' AirNow data parameters include at least the following list:
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
#' Setting \code{parameters=NULL} will generate a separate dataframe for each of
#' the above parameters.
#'
#' @note As of 2016-12-27, it appears that hourly data are available only for
#' 2016 and not for earlier years.
#'
#' @seealso \link{airnow_downloadParseData}
#' @seealso \link{airnow_qualityControl}
#'
#' @examples
#' \dontrun{
#' airnowList <- airnow_createDataDataframes("PM2.5", 2019062500)
#' }

airnow_createDataDataframes <- function(
  parameters = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d00", tz = "UTC"),
  hours = 24
) {

  logger.debug(" ----- airnow_createDataDataframes() ----- ")

  # Create the data frame that holds multiple days of AirNow data
  airnowTbl <- airnow_downloadParseData(parameters = parameters,
                                        startdate = startdate,
                                        hours = hours)

  # > head(airnowTbl)
  # # A tibble: 6 x 9
  #   ValidDate ValidTime     AQSID           SiteName GMTOffset ParameterName ReportingUnits Value         DataSource
  #       <chr>     <chr>     <chr>              <chr>     <int>         <chr>          <chr> <dbl>              <chr>
  # 1  10/16/16     00:00 000020301         WELLINGTON        -4         PM2.5          UG/M3     5 Environment Canada
  # 2  10/16/16     00:00 000030701 AYLESFORD MOUNTAIN        -4         PM2.5          UG/M3     2 Environment Canada
  # 3  10/16/16     00:00 000040203       FOREST HILLS        -4         PM2.5          UG/M3     7 Environment Canada
  # 4  10/16/16     00:00 000040103        FREDERICTON        -4         PM2.5          UG/M3    25 Environment Canada
  # 5  10/16/16     00:00 000040207    SAINT JOHN WEST        -4         PM2.5          UG/M3     8 Environment Canada

  # ----- Data Reshaping ------------------------------------------------------

  logger.trace("Reshaping %d hours of AirNow data ...", floor(hours))

  # NOTE:  Add monitorID as AQSID + "_01" to match what is done in the
  # NOTE:  "Data Reshaping" section of airnow_createMetaDataframes().

  airnowTbl$monitorID <- paste0(airnowTbl$AQSID, "_01")

  # Get a list of parameters
  if ( is.null(parameters) ) {
    parameters <- sort(unique(airnowTbl$ParameterName))
  } else {
    # Guarantee that passed in parameters actually exist
    parameters <- dplyr::intersect(parameters, unique(airnowTbl$ParameterName))
    invalidParameters <- dplyr::setdiff(parameters, unique(airnowTbl$ParameterName))
    if ( length(invalidParameters) > 0 ) {
      logger.warn("Requested parameters not found in AirNow data: %s",
                  paste0(invalidParameters, collapse=", "))
    }
  }

  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()

  # Use dplyr and reshape2 packages to seprate the data by parameter and restructure each data frame
  for ( parameter in parameters ) {

    logger.trace("Reshaping data for %s ...", parameter)

    # Create datetime variable
    tbl <- dplyr::filter(airnowTbl, airnowTbl$ParameterName == parameter)
    datestamp <- paste0(tbl$ValidDate, ' ', tbl$ValidTime)
    tbl$datetime <- lubridate::mdy_hm(datestamp) # 'mdy_hm', not 'ymd_hm'
    # Guarantee unique rows
    tbl <- dplyr::distinct(tbl)
    # Melt and recast (convert tibbles to dataframes)
    melted <- reshape2::melt(tbl, id.vars=c('datetime','monitorID'), measure.vars=c('Value'))
    dfList[[parameter]] <- reshape2::dcast(melted, datetime ~ monitorID)

  }

  # NOTE:  Some parameters, especially those with few monitors, may not have
  # NOTE:  measurements for for every single hour. Here we guarantee that the
  # NOTE:  reshaped dataframes we return will have a row for every single hour
  # NOTE:  in a month, even if that row is filled with NAs.

  # Guarantee that all times are present by starting with a dataframe containing only a uniform time axis.
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  timeAxis <- seq(starttime, starttime + lubridate::dhours(hours-1), by = 'hours')
  hourlyDF <- data.frame(datetime=timeAxis)

  logger.trace("Putting data on a uniform time axis ...")

  for ( parameter in parameters ) {

    # Join data to uniform time axis
    dfList[[parameter]] <- suppressMessages({
      dplyr::full_join(hourlyDF, dfList[[parameter]])
    })

    # NOTE:  Check this URL for some EPA defined levels:
    # NOTE:    https://aqs.epa.gov/aqsweb/documents/codetables/aqi_breakpoints.csv

    # Assume this data has been QC'ed and let everything through
    dfList[[parameter]] <- airnow_qualityControl(dfList[[parameter]],
                                                 limits = c(-Inf,Inf))

  }

  return(dfList)

}

