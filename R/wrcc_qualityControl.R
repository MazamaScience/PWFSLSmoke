#' @keywords WRCC
#' @export
#' @title Apply Quality Control to Raw WRCC Dataframe
#' @param df single site dataframe created by wrcc_downloadData()
#' @param ... additional parameters are passed to type-specific QC functions
#' @description Various QC steps are taken to clean up the incoming raw dataframe including:
#' 
#' \enumerate{
#' \item{Convert numeric missing value flags to \code{NA}.}
#' \item{Remove measurement records with values outside of valid ranges.}
#' }
#' 
#' See the individual \code{wrcc_~QualityControl()} functions for details.
#' @return  Cleaned up dataframe of WRCC monitor data.
#' @seealso \code{\link{wrcc_EBAMQualityControl}}
#' @seealso \code{\link{wrcc_ESAMQualityControl}}

wrcc_qualityControl <- function(df, ...) {
  
  # Sanity check -- df must have a monitorType
  if ( !'monitorType' %in% names(df) ) {
    logger.error("No 'monitorType' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'monitorType' column found in 'df' dataframe."))
  }
  
  monitorType <- unique(df$monitorType)
  
  # Sanity check -- df must have only one monitorType
  if ( length(monitorType) > 1 ) {
    logger.error("Multilpe monitor types found in 'df' dataframe: %s", paste0(monitorType, collapse=", "))
    stop(paste0("Multiple monitor types found in 'df' dataframe."))
  }
  
  monitorType <- monitorType[1]

  logger.debug('Applying %s QC rules', monitorType)
  
  if ( monitorType == 'BAM1020' ) {
    
    logger.warn('Dataframe contains %s data -- no QC available, original dataframe being returned', monitorType)
    
  } else if ( monitorType == 'EBAM' ) {
    
    df <- wrcc_EBAMQualityControl(df, ...)
    
  } else if ( monitorType == 'ESAM' ) {
    
    df <- wrcc_ESAMQualityControl(df, ...)
    
  } else {
    
    logger.warn('Dataframe contains %s data -- no QC available, original dataframe being returned', monitorType)
    
  }
  
  return(df)
  
}
