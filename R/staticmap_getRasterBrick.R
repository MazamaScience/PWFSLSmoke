#' @keywords plotting
#' @export
#'
#' @title Create a rasterBrick from a tiled image server
#'
#' @param centerLon Map center longitude.
#' @param centerLat Map center latitude.
#' @param maptype Defaults to \href{http://www.arcgis.com/home/item.html?id=30e5fe3149c34df1ba922e6f5bbf808f}{Esri Topographic}
#' Available to select between Stamen basemaps or Esri basemaps.\cr
#' \strong{Stamen}\cr
#' \itemize{
#'   \item terrain
#'   \item terrain-background
#'   \item terrain-labels
#'   \item terrain-lines
#'   \item toner
#'   \item toner-background
#'   \item toner-hybrid
#'   \item toner-labels
#'   \item toner-labels
#'   \item toner-lines
#'   \item toner-lite
#'   \item watercolor
#' }
#' \strong{Esri}\cr
#' \itemize{
#'   \item "world_topo"
#'   \item "world_imagery"
#'   \item "world_terrain"
#'   \item "de_Lorme"
#'   \item "world_grey"
#'   \item "world_streets"
#' }
#' @param zoom Map zoom level.
#' @param width Width of image, in pixels.
#' @param height Height of image, in pixels.
#' @param bbox If you are using the Esri maps, then the \code{bbox} parameter
#'   must be an \code{st_bbox} object as specificed in the \code{sf} package documentation
#'   \url{https://www.rdocumentation.org/packages/sf/versions/0.7-4/topics/st_bbox}.
#'   If using Stamen Maps, use a vector organized as (lonLo, latLo, lonHi, latHi)
#'   If not null,
#'   \code{centerLon}, \code{centerLat}, and \code{zoom} are ignored.
#' @param maxTiles Only utilized if selecting an esri basemap, specifies the
#'   maximum number of tiles to be returned. The greater the number,
#'   the slower the performance -- arbitrarily set to 20 by default.
#' @param crs Object of class CRS. The Coordinate Reference System (CRS) for the
#'   returned map. If the CRS of the downloaded map does not match, it will be
#'   projected to the specified CRS using \code{raster::projectRaster}.
#' @param tileCacheDir Optional location for cached tiles.
#'
#' @description Uses the input coordinates to select an appropriate method to
#' build a \code{raster::rasterBrick} object. It will either use the
#' \code{staticmap_getStamenmapBrick()} function or the
#' \code{staticmap_getEsrimapBrick()} function This can then passed as the
#' \code{rasterBrick} object to the \code{staticmap_plotRasterBrick()} function
#' for plotting.
#'
#' @note The spatial reference of the image when it is downloaded is 3857. If
#' the crs argument is different, projecting may cause the size and extent of
#' the image to differ very slightly from the input, on a scale of 1-2 pixels or
#' 10^-3 degrees.
#'
#' If bbox is specified and the bbox aspect ratio does not match the
#' width/height aspect ratio the extent is resized to prevent the map image from
#' appearing stretched, so the map extent may not match the bbox argument
#' exactly.
#'
#' @return A rasterBrick object which can be plotted with
#' \code{staticmap_plotRasterBrick()} or \code{raster::plotRGB()} and serve as a
#' base plot.
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' rasterBrick <- staticmap_getRasterBrick(-122.3318, 47.668)
#' staticmap_plotRasterBrick(rasterBrick)
#'
#'
#' rasterBrick <- staticmap_getRasterBrick(-122.3318, 47.668, "world_streets", 12)
#' staticmap_plotRasterBrick(rasterBrick)
#'
#'
#' rasterBrick <- staticmap_getRasterBrick(-122.3318, 47.668, "watercolor", 12)
#' staticmap_plotRasterBrick(rasterBrick)
#'
#' }, silent = FALSE)
#' }
#' @seealso \code{\link{staticmap_getStamenmapBrick}}
#' @seealso \code{\link{staticmap_getEsrimapBrick}}
#' @seealso \code{\link{staticmap_plotRasterBrick}}

staticmap_getRasterBrick <- function(
  centerLon = NULL,
  centerLat = NULL,
  maptype = "world_topo",
  zoom = 12,
  width = 640,
  height = 640,
  bbox = NULL,
  maxTiles = 40,
  crs = sp::CRS("+init=epsg:4326"),
  tileCacheDir = tempdir()
) {

  # ===== DEBUGGING ============================================================

  if ( FALSE ) {

    centerLon = -117.2554
    centerLat = 47.68013
    maptype = "world_topo"
    zoom = 10
    width = 640
    height = 640
    bbox = NULL
    maxTiles = 20
    crs = sp::CRS("+init=epsg:4326")
    tileCacheDir = tempdir()

  }

  # ----- Valid Presets --------------------------------------------------------

  esriMapTypes <- c("world_topo", "world_imagery", "world_terrain",
                    "de_Lorme", "world_gray", "world_streets")

  stamenMapTypes <- c("terrain", "terrain-background",
                      "terrain-labels", "terrain-lines",
                      "toner", "toner-background", "toner-hybrid",
                      "toner-labels","toner-lines", "toner-lite",
                      "watercolor")

# Call the proper RasterBrick function depending on which
# preset list the maptype param belongs to. Pass along appropriate parameters.
  if ( maptype %in% esriMapTypes ) {

    rasterBrick <- staticmap_getEsrimapBrick(
      centerLon = centerLon,
      centerLat = centerLat,
      maptype = maptype,
      zoom = zoom,
      bbox = bbox,
      width = width,
      height = height,
      maxTiles = maxTiles,
      crs = crs
    )

  } else if ( maptype %in% stamenMapTypes ) {

      rasterBrick <- staticmap_getStamenmapBrick(
        centerLon = centerLon,
        centerLat = centerLat,
        maptype = maptype,
        zoom = zoom,
        bbox = bbox,
        width = width,
        height = height,
        crs = crs
    )

  } else {

    stop(paste0("Required parameter maptype = '", maptype, "' is not recognized."))

  }

  return(rasterBrick)
}
