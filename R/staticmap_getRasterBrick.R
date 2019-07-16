#' @keywords plotting
#' @export
#'
#' @title Create a rasterBrick from a tiled image server
#'
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param baseMap defaults to \href{http://www.arcgis.com/home/item.html?id=30e5fe3149c34df1ba922e6f5bbf808f}{Esri Topographic}
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
#' @param bbox If you are using the Esri maps, then the \code{bbox} parameter must be an \code{st_bbox} object as specificed in the \code{sf} package documentation
#'   \url{https://www.rdocumentation.org/packages/sf/versions/0.7-4/topics/st_bbox}.
#'   If using Stamen Maps, use a vector organized as (lonLo, latLo, lonHi, latHi)
#'   If not null,
#'   \code{centerLon}, \code{centerLat}, and \code{zoom} are ignored.
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param maxTiles Only utilized if selecting an esri basemap, specifies the maximum number of tiles to be returned. The greater the number,
#' the slower the performance -- arbitrarily set to 20 by default.
#' @param bboxSR Only utilized if selecting a stamen basemap -- spatial reference of the bounding box.
#' @param crs object of class CRS. The Coordinate Reference System (CRS) for the
#'   returned map. If the CRS of the downloaded map does not match, it will be
#'   projected to the specified CRS using \code{raster::projectRaster}.
#'
#' @description Uses the input coordinates to select an appropriate method to build a \code{raster::rasterBrick} object.
#' It will either use the \code{staticmap_getStamenmapBrick()} function or the \code{staticmap_getEsrimapBrick()} function
#' This can then passed as the \code{rasterBrick} object to the
#' \code{staticmap_plotRasterBrick()} function for plotting.
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
#' mapRaster <- staticmap_getRasterBrick(-122.3318, 47.668)
#' staticmap_plotRasterBrick(mapRaster)
#' }
#' \dontrun{
#' mapRaster <- staticmap_getRasterBrick(-122.3318, 47.668, 12, 'world_streets')
#' staticmap_plotRasterBrick(mapRaster)
#' }
#' \dontrun{
#' mapRaster <- staticmap_getRasterBric(-122.3318, 47.668, 12, 'watercolor')
#' staticmap_plotRasterBrick(mapRaster)
#' }
#' @seealso \code{\link{staticmap_getStamenmapBrick}}
#' @seealso \code{\link{staticmap_getEsrimapBrick}}
#' @seealso \code{\link{staticmap_plotRasterBrick}}

staticmap_getRasterBrick <- function(
  centerLon = NULL,
  centerLat = NULL,
  zoom = 12,
  baseMap = "world_topo",
  bbox = NULL,
  width = 640,
  height = 640,
  maxTiles = 20,
  bboxSR = "4326",
  crs = sp::CRS("+init=epsg:4326")
) {

  # ----- Valid Presets --------------------------------------------------
  ESRI_MAP_TYPES <- c("world_topo", "world_imagery", "world_terrain", "de_Lorme", "world_gray", "world_streets")

  STAMEN_MAP_TYPES <- c("terrain", "terrain-background",
                    "terrain-labels", "terrain-lines",
                    "toner", "toner-background", "toner-hybrid",
                    "toner-labels","toner-lines", "toner-lite",
                    "watercolor")

# Call the proper RasterBrick function depending on which
# preset list the baseMap param belongs to. Pass along appropriate parameters.
  if ( baseMap %in% ESRI_MAP_TYPES ) {
    brickOut <- staticmap_getEsrimapBrick(
      centerLon=centerLon,
      centerLat=centerLat,
      zoom=zoom,
      baseMap=baseMap,
      bbox=bbox,
      width=width,
      height=height,
      maxTiles=maxTiles,
      crs=crs
    )
  } else if ( baseMap %in% STAMEN_MAP_TYPES ) {
      brickOut <- staticmap_getStamenmapBrick(
        centerLon=centerLon,
        centerLat=centerLat,
        maptype=baseMap,
        zoom=zoom,
        bbox=bbox,
        width=width,
        height=height,
        bboxSR=bboxSR,
        crs=crs
    )
  } else {
    stop(paste0("Required parameter baseMap = '", baseMap, "' is not recognized."))
  }

  return(brickOut)
}
