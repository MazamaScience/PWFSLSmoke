#' @keywords plotting
#' @export
#' @title Add AQI Lines to a Plot
#' @param ... additional arguments to be passed to \code{abline()}
#' @description This function is a convenience for:
#'
#' \code{abline(h = AQI$breaks_24, col = AQI$colors)}

addAQILines <- function(...) {

  graphics::abline(h = AQI$breaks_24, col = AQI$colors, ...)

}
