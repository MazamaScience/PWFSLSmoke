#' @keywords plotting
#' @export
#'
#' @title Plot an RGB rasterBrick
#'
#' @param rasterBrick an RGB rasterBrick object. It is assumed that layer 1
#' represents red, layer 2 represents green, and layer 3 represents blue.
#' @param grayscale logical specifying conversion to grayscale
#' @param ... arguments passed on to \code{raster::plot()}
#' (for \code{grayscale = TRUE}) or \code{raster::plotRGB()}
#' (for \code{grayscale = FALSE})
#'
#' @description Plots the incoming \code{rasterBrick}.
#'
#' @examples
#' \dontrun{
#' rasterBrick <- staticmap_getStamenmapBrick(-122.3318, 47.668)
#' staticmap_plotRasterBrick(rasterBrick)
#' staticmap_plotRasterBrick(rasterBrick, grayscale = TRUE)
#' }
#'
#' @seealso \code{\link{staticmap_getStamenmapBrick}}

staticmap_plotRasterBrick <- function(
  rasterBrick = NULL,
  grayscale = FALSE,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  if ( is.null(rasterBrick) )
    stop("Required parameter 'rasterBrick' is missing")

  argsList <- list(...)

  if ( is.null(argsList$interpolate) )
    argsList$interpolate <- TRUE


  # ----- Plot RasterBrick -----------------------------------------------------

  if ( !grayscale ) {

    argsList$x <- rasterBrick
    do.call(raster::plotRGB, argsList)

  } else {

    singleLayerMap <- raster::raster(rasterBrick, layer = 1)
    grayImageArray <- round(apply(raster::values(rasterBrick), 1, sum)/3)
    raster::setValues(singleLayerMap, grayImageArray)
    argsList$x <- singleLayerMap
    if ( is.null(argsList$col) ) {
      argsList$col <- grDevices::gray.colors(255, gamma = 1)
    }
    argsList$axes = FALSE
    argsList$xlab = ''
    argsList$ylab = ''
    do.call(raster::plot, argsList)

  }

}
