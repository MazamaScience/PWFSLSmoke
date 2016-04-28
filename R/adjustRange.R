#' @keywords internal
#' @export
#' @title Create a Scaled Range
#' @param x vector of numeric values
#' @param factor scaling factor to be applied
#' @param minRange minimum range to be returned
#' @description Scale the range of values by a given factor. The min and max values of the 
#' original range of valus are adjusted so that the new range is larger or smaller by \code{factor}.
#' The returned range will always be equal to or greater than \code{minRange}.
#' @return low and high values of the adjusted range
#' @examples
#' \dontrun{
#' x <- (1:10)
#' range(x)
#' [1] 1 10
#' x_new <- adjustRange(x, factor = 2)
#' range(x_new)
#' [1] -3.5 14.5
#' }
adjustRange <- function(x, factor=1.1, minRange=0.1) {
  scaledWidth <- diff(range(x)) * (factor - 1)
  low <- range(x)[1] - scaledWidth / 2
  high <- range(x)[2] + scaledWidth / 2
  if ( (high - low) < minRange ) {
    mid <- (max(x)-min(x))/2 + min(x)
    low <- mid - minRange / 2
    high <- mid + minRange / 2
  }
  return(c(low, high))
}
