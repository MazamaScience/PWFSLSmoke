#' @keywords OpenAQ
#' @export
#' @title Create OpenAQ Data Dataframe
#' @param df a OpenAQ dataframe after metadata enhancement
#' @param parameters pollutant name
#' @description After addtional columns(i.e. \code{'datetime'}, \code{'stateCode'}, and \code{'monitorID'}) 
#' have been applied to an OpenAQ dataframe,
#' we can extract the PM2.5 values and store them in a \code{data} dataframe
#' organized as time-by-monitor.
#' 
#' The first column of the returned dataframe is named \code{'datetime'} and
#' contains a \code{POSIXct} time in UTC. Additional columns contain data
#' for each separate monitorID. 
#' 
#' Available parameters are limited to:
#' PM2.5, PM10, ozone (O3), sulfur dioxide (SO2), nitrogen dioxide (NO2), 
#' carbon monoxide (CO), and black carbon (BC)
#' \enumerate{
#' \item{pm25}{ -- PM2.5}
#' \item{pm10}{ -- PM10}
#' \item{o3}{ -- ozone }
#' \item{so2}{ -- sulfer dioxide }
#' \item{no2}{ -- nitrogen dioxide}
#' \item{co}{ -- carbon monoxide}
#' \item{bc}{ -- black carbon}
#' }
#' 
#' @return A \code{data} dataframe for use in a emph{ws_monitor} object.


openaq_createDataDataframes <- function(df, parameters=NULL){
  
  # Sanity check -- df must have a monitorID
  if ( !'monitorID' %in% names(df) ) {
    logger.error("No 'monitorID' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'monitorID' column found in 'df' dataframe."))
  }
  
  # Sanity check -- df must have datetime
  if ( !'datetime' %in% names(df) ) {
    logger.error("No 'datetime' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'datetime' column found in 'df' dataframe."))
  }
  
  # Get a list of parameters
  if ( is.null(parameters) ) {
    parameters <- sort(unique(df$parameter))
  } else {
    # Guarantee that passed in parameters actually exist
    parameters <- dplyr::intersect(parameters, unique(df$parameter))
    invalidParameters <- dplyr::setdiff(parameters, unique(df$parameter))
    if ( length(invalidParameters) > 0 ) {
      logger.warn("Requested parameters not found in OpenAQ data: %s", paste0(invalidParameters, collapse=", "))
    }
  }
  
  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()
 
  # Use dplyr and reshape2 packages to seprate the data by parameter and restructure each data frame
  for (parameter in parameters) {
    
    logger.debug("Reshaping data for %s ...", parameter)
    
    # extract the disired parameter and columns from the dataframe
    indexes <- which(df$parameter == parameter)
    subDF <- df[indexes,]
    subDF <- subDF[,c("datetime","value","monitorID")]

    # Melt and recast
    melted <- reshape2::melt(subDF, id.vars=c('datetime','monitorID'))
    
    # check if it's exactly one measurement per hour at each location
    countValuePerHr <- reshape2::dcast(melted,datetime~monitorID,length)
    maxCount <- max(countValuePerHr[,-1], na.rm=TRUE)
    if (maxCount>1) logger.warn("Up to %s measurements per hour -- median used",maxCount)
    
    # create a dataframe for hours to be used later
    hourlyDF <- data.frame(seq(min(melted$datetime, na.rm=TRUE), max(melted$datetime, na.rm=TRUE), by="hours"))
    names(hourlyDF) <- "datetime"
    
    # create a dataframe for values
    parameterDF <- reshape2::dcast(melted,datetime~monitorID,stats::median)
    
    # combine the two dataframes together by doing a left join
    dataDF <- dplyr::left_join(hourlyDF,parameterDF,by="datetime")

    dfList[[parameter]] <- dataDF
    logger.debug("Created 'data' dataframe for parameter %s with %d rows and %d columns", parameter, nrow(dataDF), ncol(dataDF))
    
  }

  return(dfList)
}

