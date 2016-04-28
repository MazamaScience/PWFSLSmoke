#' @keywords AirNow
#' @export
#' @title Return Reshaped, Monthly Dataframes of AirNow Data
#' @param user user name
#' @param pass pass
#' @param yearMonth desired year and month  (integer or character representing YYYYMM)
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical requesting verbose output from libcurl
#' @param debug logical specyfing whether to print out additional debugging lines
#' @description The airnow_createDataDataframe() function uses the \link{airnow_downloadData} function 
#' to download monthly dataframes of AirNow data and restructures that data into a format that is compatible
#' with the PWFSLSmoke package \emph{ws_monitor} data model.
#' 
#' The function also has built in 
#' post-processing script that returns both raw and processed data.
#' @return Both raw and processed list of data frames where each of them is the data table for the unique parameter (e.g: PM2.5, NOX)
#' @seealso \link{airnow_downloadData}
#' @seealso \link{airnow_downloadMonthlyData}
#' @examples
#' \dontrun{
#' AirnowData <- airnow_createDataDataframe(user, pass, 201507)
#' }

airnow_createDataDataframe <- function(user, pass, yearMonth, tries=6, verbose=FALSE, debug=FALSE) {
  
#   # ----- Data Download -------------------------------------------------------
#   
#   if (debug) print(paste0('Downloading AirNow data for ',yearMonth,' ...'))
#   
#   # Calculate the number of days in the month of the interest
#   startdate <- paste0(yearMonth, '0100')
#   starttime <- lubridate::ymd_h(startdate)
#   dayCount <- as.numeric(lubridate::days_in_month(starttime))
#   
#   # Create the data frame that holds a month worth airnow data
#   airnowRaw <- airnow_downloadData(user, pass, startdate=startdate, days=dayCount, tries=tries, verbose=verbose, debug=debug)
#   
#   
#   # ----- Data Reshaping ------------------------------------------------------
#   
#   if (debug) print(paste0('Reshaping AirNow data for ',yearMonth,' ...'))
# 
#   # NOTE:  Example lines from the aggregated dataframe:
#   # NOTE:
#   # NOTE:    ValidDate ValidTime     AQSID   SiteName GMTOffset ParameterName ReportingUnits Value                  DataSource
#   # NOTE:  1  08/01/14     00:00 000010102 St. John's        -4         OZONE            PPB    12 Newfoundland & Labrador DEC
#   # NOTE:  2  08/01/14     00:00 000020301 WELLINGTON        -4           NO2            PPB     0          Environment Canada
#   # NOTE:  3  08/01/14     00:00 000020301 WELLINGTON        -4         OZONE            PPB    18          Environment Canada
#   # NOTE:  4  08/01/14     00:00 000020301 WELLINGTON        -4         PM2.5          UG/M3     7          Environment Canada
#   # NOTE:  5  08/01/14     00:00 000020301 WELLINGTON        -4            NO            PPB     0          Environment Canada
#   
#   # Get a list of parameters
#   parameters <- sort(unique(airnowRaw$ParameterName))
#   
#   # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
#   dataReshaped <- list()
#   
#   # Use dplyr and reshape2 packages to seprate the data by the types and restructure each data frame
#   # TODO: creating static timeaxis for each parameter for the sanity check 
#   for (parameter in parameters) {
#     
#     # Create datetime variable
#     df <- dplyr::filter(airnowRaw, airnowRaw$ParameterName == parameter)
#     datestamp <- paste0(df$ValidDate, ' ', df$ValidTime)
#     df$datetime <- lubridate::mdy_hm(datestamp) # 'mdy_hm', not 'ymd_hm'
#     # Guarantee unique rows
#     df <- dplyr::distinct(df)
#     # Melt and recast 
#     melted <- reshape2::melt(df, id.vars=c('datetime','AQSID'), measure.vars=c('Value')) 
#     dataReshaped[[parameter]] <- reshape2::dcast(melted, datetime ~ AQSID)
#     
#   }
#   
#   # NOTE:  Some parameters, especially those with few monitors, may not have measurements for
#   # NOTE:  for every single hour. Here we guarantee that the reshaped dataframes we return will
#   # NOTE:  have a row for every single hour in a month, even if that row is filled with NAs.
#   
#   # Guarantee that all times are found by merging a uniform time axis dataframe with AirNow dataframes
#   timeAxis <- seq(starttime, starttime + lubridate::ddays(dayCount), by='hours')
#   timeDF <- data.frame(datetime=timeAxis)
#   for (parameter in parameters) {
#     dataReshaped[[parameter]] <- suppressMessages(dplyr::full_join(timeDF, dataReshaped[[parameter]]))
#   }
#   
#   
#   # ----- Post-Processing: Quality Control ------------------------------------
#   
#   if (debug) print(paste0('Applying QC to AirNow data for ',yearMonth,' ...'))
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
#   # Go through the data in each parameter and compute the number of -999.0s and out of range values
#   for (parameter in parameters) {
#       
#     print(parameter)
# 
#     # Extract the data portion only (omitting 'datetime')
#     dataMatrix <- dataReshaped[[parameter]][,-1]
#     
#     # Count the -999s for the QC report
#     minus999Count <- length(dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)])
# 
#     # First convert -999.0s to NAs
#     dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)] <- NA
# 
#     # Replace the data in dataReshaped
#     dataReshaped[[parameter]][,-1] <- dataMatrix
#     
#     # TODO:  Deal with extra parameters like 'EC'
# 
#     if ( (parameter %in% loLimit) && (parameter %in% hiLimit) ) {
#       # Count the number of values outside the range of acceptable values
#       loCount <- length(dataMatrix[dataMatrix < loLimit[[parameter]] & !is.na(dataMatrix)])
#       hiCount <- length(dataMatrix[dataMatrix > hiLimit[[parameter]] & !is.na(dataMatrix)])
#     } else {
#       warning(paste0('Parameter ',parameter,' has no limits defined.'))
#       loCount <- 0
#       hiCount <- 0
#     }
#     
#     # NOTE:  If the parameter dataframe only has two columns: c('datetime','<monitorID>'), dropping
#     # NOTE:  the first column with [-1] will return a numeric vector rather than a dataframe. We
#     # NOTE:  need to treat this cases separately.
#     
#     if (class(dataMatrix) == 'numeric') {
#       
#       if (loCount==0 & hiCount==0) {
#         badMonitors <- NULL
#       } else {
#         badMonitors <- colnames(dataReshaped[[parameter]])[2]
#       }
#       
#     } else {
#       
#       # Find the monitor IDs that measured such values
#       if (loCount==0 & hiCount!=0) {
#         
#         badCols <- unique(which(dataMatrix > hiLimit[[parameter]] & !is.na(dataMatrix), arr.ind=TRUE)[,2])
#         badMonitors <- colnames(dataMatrix)[badCols]
#         
#       } else if (loCount!=0 & hiCount==0) {
#         
#         badCols <- unique(which(dataMatrix < loLimit[[parameter]] & !is.na(dataMatrix), arr.ind=TRUE)[,2])
#         badMonitors <- colnames(dataMatrix)[badCols]
#         
#       } else if (loCount==0 & hiCount==0) {
#         
#         badMonitors <- NULL
#         
#       } else {
#         
#         loCols <- unique(which(dataMatrix < loLimit[[parameter]] & !is.na(dataMatrix), arr.ind=TRUE)[,2])
#         hiCols <- unique(which(dataMatrix > hiLimit[[parameter]] & !is.na(dataMatrix), arr.ind=TRUE)[,2])
#         badCols <- dplyr::union(loCols, hiCols)
#         badMonitors <- colnames(dataMatrix)[badCols]
#       
#       }
#     }
#     
#     # Fill up the QCInfoList report with the information obtianed above 
#     QCInfoList[[parameter]]$'num_OutofRange' <- loCount+hiCount
#     QCInfoList[[parameter]]$'OutofRange_Monitors' <- badMonitors
#     QCInfoList[[parameter]]$'num_Converted2NAs' <- minus999Count+loCount+hiCount
#       
#   }
#   
#   # Get ready to store raw and QC data separately
#   dataRaw <- dataReshaped
#   dataQC <- dataReshaped
# 
#   # Generate the QC version of the data
#   for (parameter in parameters) {
#     
#     dataMatrix <- dataQC[[parameter]][,-1]
#     if ( (parameter %in% loLimit) && (parameter %in% hiLimit) ) {
#       dataMatrix[dataMatrix < loLimit[[parameter]] & !is.na(dataMatrix)] <- NA
#       dataMatrix[dataMatrix > hiLimit[[parameter]] & !is.na(dataMatrix)] <- NA
#     }
#     dataQC[[parameter]][,-1] <- dataMatrix
#       
#   }
#       
#   # TODO:  Add a small loop to print out QCInfoList with cat() or print()
#   if (debug) print(paste0('Printing out QCInfo for ',yearMonth,' ...'))
#   
#   if (debug) str(QCInfoList, give.attr=FALSE, give.length=FALSE, give.head=FALSE)
#   
#   dataList <- list(dataRaw=dataRaw,
#                    dataQC=dataQC,
#                    QCInfoList=QCInfoList)
# 
#   return(dataList)

}
  
