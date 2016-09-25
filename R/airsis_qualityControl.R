#' @keywords AIRSIS
#' @export
#' @title Apply Quality Control to Raw AIRSIS Dataframe
#' @param df single site dataframe created by airsis_downloadData()
#' @description Various QC steps are taken to clean up the incoming raw dataframe including:
#' 
#' \enumerate{
#' \item{Ensure GPS location data are included in each measurement record.}
#' \item{Remove GPS location records.}
#' \item{Remove measurement records with values outside of valid ranges.}
#' }
#' 
#' See the individual \code{airsis_~QualityControl()} functions for details.
#' @return  Cleaned up dataframe of AIRSIS monitor data.
#' @seealso \code{\link{airsis_EBAMQualityControl}}

airsis_qualityControl <- function(df) {
  
  monitorTypeList <- airsis_identifyMonitorType(df)
  monitorType <- monitorTypeList$monitorType

  logger.debug('Applying %s QC rules', monitorType)
  
  if ( monitorType == 'BAM1020' ) {
    
    logger.warn('Dataframe contains %s data -- no QC available, original dataframe being returned', monitorType)
    
  } else if ( monitorType == 'EBAM' ) {
    
    df <- airsis_EBAMQualityControl(df)
    
  } else if ( monitorType == 'ESAM' ) {
    
    # NOTE:  Conversation with Sim and Lee on 2015-07-09. Accept all values of RHi for now
    df <- airsis_ESAMQualityControl(df, valid_RHi=c(-Inf,Inf))
    
  } else {
    
    logger.warn('Dataframe contains %s data -- no QC available, original dataframe being returned', monitorType)
    
  }
  
  return(df)
  
}
