#' @keywords AirNow
#' @export
#' @title Apply Quality Control to AirNow Dataframe
#' @param df multi-site restructured dataframe created within \code{airnow_createDataDataframe()}
#' @param limits lo and hi range of valid values
#' @description Perform range validation on AirNow data. This function also replaces values 
#' of \code{-999} with \code{NA}.
#' @return Cleaned up dataframe of AIRSIS monitor data.
#' @seealso \link{airnow_createDataDataframes}

airnow_qualityControl <- function(df, limits=c(-Inf,Inf)) {
  
  # Extract the data portion only (omitting 'datetime')
  dataMatrix <- df[,-1]
  
  # Count the -999s for logging
  minus999Count <- length(dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)])
  if (minus999Count > 0) logger.debug("Replacing %d values of -999 with NA",minus999Count)
  
  # Convert -999.0s to NAs
  dataMatrix[dataMatrix == -999.0 & !is.na(dataMatrix)] <- NA
  
  # Count the out of range values for logging
  outOfRangeLoCount <- length(dataMatrix[dataMatrix < limits[1] & !is.na(dataMatrix)])
  outOfRangeHiCount <- length(dataMatrix[dataMatrix > limits[2] & !is.na(dataMatrix)])
  if (outOfRangeLoCount > 0) logger.debug("Replacing %d values below %s with NA",outOfRangeLoCount,limits[1])
  if (outOfRangeHiCount > 0) logger.debug("Replacing %d values above %s with NA",outOfRangeHiCount,limits[2])
  
  # Convert out of range values to NAs
  dataMatrix[dataMatrix < limits[1] & !is.na(dataMatrix)] <- NA
  dataMatrix[dataMatrix > limits[2] & !is.na(dataMatrix)] <- NA
  
  # Replace the data
  df[,-1] <- dataMatrix
  
  return(df)

}
  
