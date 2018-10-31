#' @title Color Palettes for Air Quality Monitoring Data
#'
#' @description Creates a *leaflet* color palette function that can be used
#' to convert monitoring data into vectors of colors.
#'
#' @param style Palette style, one of `aqi`.
#' @param reverse Logicial specifying whether the colors (or color function) in
#' palette should be used in reverse order.
#'
#' @return A function that takes a single parameter x; when called with a vector
#' of numbers, #RRGGBB color strings are returned.
#'
#' @seealso `leaflet::colorBin()`
#'
#' @export
#'
#' @examples
# layuout(matrix(seq(2)))
#' pm25 <- PWFSLSmoke::Carmel_Valley$data[,2]
#' binned_colors <- aqiPalette("aqi")(pm25)
# continuous_colors <- aqiPalette("aqi_numeric")(pm25)
#' plot(pm25, col=binned_colors, pch=15, main='Binned Colors')
# plot(pm25, col=continuous_colors, pch=15, main='Continuous Colors')
# layout(1)

aqiPalette <- function(style = "aqi",
                       reverse = FALSE) {

  # Validate style
  validStyles <- c("aqi")#, "aqi_numeric")
  if ( !(style %in% validStyles) ) {
    stop(
      paste0(
        "'", style, "' is not a valid 'style' \n",
        "Please choose from: ", paste0(validStyles, collapse = ", ")
      )
    )
  }

  # Create palette
  if ( style == "aqi" ) {
    palette <- leaflet::colorBin(palette = AQI$colors,
                                 bins = AQI$breaks_24,
                                 na.color = "#bbbbbb",
                                 alpha = FALSE,
                                 reverse = reverse)
    # } else if ( style == "aqi_numeric" ) {
    # TODO:  Generate a continuous color palette which touches AQI colors at
    # TODO:  the AQI break points.
    #   palette <- leaflet::colorNumeric(palette = AQI$colors,
    #                                    bins = AQI$breaks_24,
    #                                    na.color = "#bbbbbb")
  }

  return(invisible(palette))

}
