#' @keywords internal
#' @export
#' @title Download URL with Multiple Attempts
#' @param url string with the URI
#' @param curl previously initialized CURL context/handle (see RCurl::getURL())
#' @param .opts named list of \code{CURLOptions} (see RCurl::getURL())
#' @param .encoding string identifying the encoding (see RCurl::getURL())
#' @param tries number of times to retry in the face of timeouts
#' @description This internal function provides a wrapper for RCurl::getURL() that mimics the
#' behavior of the unix \code{wget} command with the \code{--tries} flag. If a download fails
#' with an error message of \code{'Timeout was reached'} the function will sleep an increasing
#' amount of time before retrying the download up to \code{tries} times.
#' 
#' The amount of time to sleep stats at one second is increased by one second for each attempted download.
#' @return The text that is the HTTP response. (see RCurl::getURL())
#' @references \url{http://www.omegahat.org/RCurl}

retryURL <- function(url, curl=RCurl::getCurlHandle(),
                    .opts=list(), .encoding="UTF-8", tries=6) {
  
  # TODO:  Ran into an issue one time where things got stuck here and never progressed:
  # TODO:
  # TODO:  > df <- airnow_downloadHourlyData(USER,PASS, 2015070112, verbose=TRUE)
  # TODO:  *   Trying 24.104.117.9...
  # TODO:  * Connected to ftp.airnowapi.org (24.104.117.9) port 21 (#0)
  # TODO:  < 220 Service ready for new user.
  # TODO:  > USER USER
  # TODO:
  # TODO:  What curl 'timeout' options would allow this to return with an intelligible error?
  
  attempt <- 1
  while ( attempt <= tries ) {
    result <- try( fileText <- RCurl::getURL(url, curl=curl, .opts=.opts, .encoding="UTF-8") )
    if ( class(result) == "try-error" ) {
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg, 'Timeout was reached') ) {
        Sys.sleep(attempt)
        attempt <- attempt + 1
        next
      } else {
        stop(err_msg)
      }
    } else {
      break
    }
  }
  
  if ( class(result) == "try-error" ) {
    err_msg <- paste('retryURL(...) failed after',tries,'tries')
    stop(err_msg)
  } else {
    return(fileText)
  }
  
}
