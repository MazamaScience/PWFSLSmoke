#' @keywords AirNow
#' @export
#' @title Apply Quality Control to AirNow Dataframe
#' @param df multi-site restructured dataframe created within airsis_createDataDataframe()
#' @param limits lo and hi range of valid values
#' @description Perform range validation on AirNow data.
#' # TODO:  Include all ranges here
#' @return Cleaned up dataframe of AIRSIS monitor data.
#' @seealso \link{airnow_createDataDataframe}

airnow_qualityControl <- function(df, limits=c(-Inf,Inf)) {
  
  # TODO:  Clean up this function
  
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
#   # Go through the data in each parameter and compute the number of -999.0s and out of range values
#   for (parameter in parameters) {
#       
#     print(parameter)
# 
#     # Extract the data portion only (omitting 'datetime')
#     dataMatrix <- dfList[[parameter]][,-1]
#     
#     # Count the -999s for the QC report
#     minus999Count <- length(dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)])
# 
#     # First convert -999.0s to NAs
#     dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)] <- NA
# 
#     # Replace the data in dfList
#     dfList[[parameter]][,-1] <- dataMatrix
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
#         badMonitors <- colnames(dfList[[parameter]])[2]
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
#   dataRaw <- dfList
#   dataQC <- dfList
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
   
  # Extract the data portion only (omitting 'datetime')
  dataMatrix <- df[,-1]
  
  # Count the -999s for logging
  minus999Count <- length(dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)])
  if (minus999Count > 0) logger.debug('Replacing %d values of -999 with NA',minus999Count)
  
  # Convert -999.0s to NAs
  dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)] <- NA
  
  # Count the out of range values for logging
  outOfRangeLoCount <- length(dataMatrix[dataMatrix < limits[1] & !is.na(dataMatrix)])
  outOfRangeHiCount <- length(dataMatrix[dataMatrix > limits[2] & !is.na(dataMatrix)])
  if (outOfRangeLoCount > 0) logger.debug('Replacing %d values below %s with NA',outOfRangeLoCount,limits[1])
  if (outOfRangeHiCount > 0) logger.debug('Replacing %d values above %s with NA',outOfRangeHiCount,limits[2])
  
  # Convert out of range values to NAs
  dataMatrix[dataMatrix < limits[1] & !is.na(dataMatrix)] <- NA
  dataMatrix[dataMatrix > limits[2] & !is.na(dataMatrix)] <- NA
  
  # Replace the data
  df[,-1] <- dataMatrix
  
  return(df)

}
  
