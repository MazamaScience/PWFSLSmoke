#' @keywords plotting
#' @importFrom graphics polygon
#' @export
#' @title Add a Colored Polygon to a Plot
#' @param x x location of center
#' @param y y location of center
#' @param sides number of sides
#' @param radius radius
#' @param rotation amount to rotate the polygon in radians
#' @param border border color (see \code{?polygon})
#' @param col fill color (see \code{?polygon})
#' @param ... additional arguments to be passed to \code{polygon()}
#' @description Add a multi-sided polygon to a plot.
#' @examples
#' # Create AQI dots
#' plot(1:6, rep(0,6), xlim=c(-1,7), ylim=c(-1,3),
#'     axes=FALSE, xlab='', ylab='', col='transparent')
#' for (i in 1:6) {
#'   addPolygon(i, 2, 72, 0.4, 0, col=PWFSLSmoke::AQI$colors[i])
#'   addPolygon(i, 1, 4, 0.4, pi/4, co=PWFSLSmoke::AQI$colors[i])
#'   addPolygon(i, 0, 3, 0.4, pi/2, col=PWFSLSmoke::AQI$colors[i])
#' }

addPolygon <- function(x = 0,
                       y = 0,
                       sides = 72,
                       radius = 1,
                       rotation = 0,
                       border = NULL,
                       col = NA,
                       ...) {

  # TODO:  Handle vector arguments

  xx <- x + radius * cos(rotation + 2 * pi * (1:(sides + 1)) / sides)
  yy <- y + radius * sin(rotation + 2 * pi * (1:(sides + 1)) / sides)

  polygon(xx, yy, border = border, col = col, ...)

}
