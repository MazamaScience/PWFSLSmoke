#' @keywords AIRSIS
#' @export
#' @title Read and Parse Single Monitor Data from AIRSIS
#' @param file either a path to a AIRSIS data file the contents of the file as a character string
#' @param monitorName human readable monitor identifier
#' @param verbose logical flag to generate verbose output
#' @description An AIRSIS monitor data file is read parsed and the results are returned as a dataframe.
#' @return Dataframe of AIRSIS monitor data.
#' @references \href{http://app.airsis.com/usfs/}{Interagency Real Time Smoke Monitoring}

airsis_readData <- function(file, monitorName=NULL, verbose=FALSE) {
  
  if (is.null(monitorName)) stop(paste0('A monitorName must be passed in as this cannot be determined from the file contents.'))
  
  # Data are expected to be in a nice CSV file of the following format:
  
  #   Date,Time (pdt),ConcRT (?g/m?),ConcHR (?g/m?),Flow (l/m),Wind Speed (m/s),Wind Direction (?),Ambient Temp (C),Ambient Temp (F),RH external (%),RH internal (%),Battery Voltage (volts),Filter Temp (C),Filter Temp (F),Alarm,Type
  #   8/21/2012,11:00,-5,-5,16.7,0.4,299,14.7,58.46,70,29,14.3,22.9,73.22,0,PM 2.5
  #   8/21/2012,12:00,12,18,16.7,0.9,265,14.8,58.64,70,30,14.3,23.6,74.48,0,PM 2.5
  #   ...

  df <- readr::read_csv(file)

  # NOTE:  The header uses at least the following unicode characters
  # NOTE:  * MICRO "\u00B5"
  # NOTE:  * SUPERSCRIPT THREE "\u00B3"
  # NOTE:  * DEGREE SIGN "\u00B0"
  # NOTE:
  # NOTE:  Just to be careful, we'll convert these before processing further
  
  names(df) <- iconv( names(df), from='LATIN1', to='UTF-8' )

  # Add monitor name
  df$monitorName <- monitorName
  
  return(df)
  
}
