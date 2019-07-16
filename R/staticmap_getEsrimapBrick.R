#' @keywords plotting
#' @export
#'
#' @title Create a rasterBrick from an Esri tiled image server
#'
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param baseMap selects the appropriate Esri tile server. Options include:
#' \itemize{
#'   \item "world_topo"
#'   \item "world_imagery"
#'   \item "world_terrain"
#'   \item "de_Lorme"
#'   \item "world_grey"
#'   \item "world_streets"
#' }
#' @param bbox must be an \code{st_bbox} object as specificed in the \code{sf} package documentation
#'   \url{https://www.rdocumentation.org/packages/sf/versions/0.7-4/topics/st_bbox}. If not null,
#'   \code{centerLon}, \code{centerLat}, and \code{zoom} are ignored.
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param maxTiles maximum number of tiles to be returned. The greater the number, the slower the performance -- arbitrarily set to 20 by default.
#' @param crs object of class CRS. The Coordinate Reference System (CRS) for the
#'   returned map. If the CRS of the downloaded map does not match, it will be
#'   projected to the specified CRS using \code{raster::projectRaster}.
#'
#' @description Uses the input coordinates to fetch and composite a raster from the tile server. Returns a
#' \code{raster::rasterBrick} object. This
#' can then passed as the \code{rasterBrick} object to the
#' \code{staticmap_plotRasterBrick()} function for plotting.
#'
#' As of July 2019, this list is a handy reference to the freely available tile
#' servers which can be previewed at the following URL:
#'
#' \url{https://leaflet-extras.github.io/leaflet-providers/preview/}
#'
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
#' @note Currently Esri tile servers are the only functional ones utilizing this paradigmn.
#' Using the \href{https://github.com/MilesMcBain/slippymath}{slippymath} package, only the freely
#' available Esri tile servers compile to a properly colored RasterBrick.
#'
#' @return A rasterBrick object which can be plotted with
#' \code{staticmap_plotRasterBrick()} or \code{raster::plotRGB()} and serve as a
#' base plot.
#'
#' @examples
#' \dontrun{
#' mapRaster <- staticmap_getEsrimapBrick(-122.3318, 47.668)
#' staticmap_plotRasterBrick(mapRaster)
#' }
#' @seealso \code{\link{staticmap_getRasterBrick}}
#' @seealso \code{\link{staticmap_plotRasterBrick}}

staticmap_getEsrimapBrick <- function(
  centerLon = NULL,
  centerLat = NULL,
  zoom = 12,
  bbox = NULL,
  baseMap = NULL,
  width = 640,
  height = 640,
  maxTiles = 20,
  crs = sp::CRS("+init=epsg:4326")
) {
  # ----- Determine bounding box -----------------------------------------------

  # Projection information:
  # https://spatialreference.org/
  # https://en.wikipedia.org/wiki/Web_Mercator_projection

  # EPSG:4326 -- lon lat pairs on the WGS84 reference ellipsoid
  # EPSG:3857 -- most (all?) tiling servers have a "web Mercator" projection

  # Calculate degrees per pixel from zoom to determine bbox:
  # * google maps tiles are 256x256 pixels
  # * zoom level 0 includes 360 degrees-EW
  # * every time zoom level increases, scale halves. Thus, for degrees EW:
  # pixels/degree = 256pixels*2^zoomLevel/360degrees
  # degrees/pixel = 360degrees/(256pixels*2^zoomLevel)
  # degrees-NS/pixel = degreesPerPixelNS*cos(latitude)
  # Source: https://gis.stackexchange.com/questions/7430/what-ratio-scales-do-google-maps-zoom-levels-correspond-to

  # Fudge factor
  adjustedZoom <- zoom + 0

  # Create lon/lat ranges
  if ( !is.null(centerLon) && !is.null(centerLat) ) {
    degreesPerPixelEW <- 360/(256 * 2^adjustedZoom)
    degreesPerPixelNS <- degreesPerPixelEW*cos(centerLat * pi/180) # radians
    xmin <- centerLon - degreesPerPixelEW*(width/2 - 0.5)
    xmax <- centerLon + degreesPerPixelEW*(width/2 - 0.5)
    ymin <- centerLat - degreesPerPixelNS*(height/2 - 0.5)
    ymax <- centerLat + degreesPerPixelNS*(height/2 - 0.5)

    outlook_bbox <-
      sf::st_bbox(c(xmin=xmin,
                    xmax=xmax,
                    ymin=ymin,
                    ymax=ymax),
                  crs = crs)
  } else {
    stop("centerLat + centerLon, or bbox must be specified")
  }

  # convert sf bbox to slippymath tile grid
  tile_grid <- slippymath::bbox_to_tile_grid(outlook_bbox, max_tiles = maxTiles)

  # ----- Toggle between Esri Tile Servers -----------------------------------------------
  # Currently only an available presets list for a user to enter, is available with no option to specify a
  # entirely unique url for a tile server. Examples can be found here https://leaflet-extras.github.io/leaflet-providers/preview/.
.
  # NOTE: Tile Servers must return a RasterBrick object from slippymath::compose_tile_grid, and not a tg_composite() (OSM)

  # Available Presets
  AVAILABLE_PRESETS <-  c("world_topo", "world_imagery", "world_terrain", "de_Lorme", "world_gray", "world_street")

  world_topo <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{zoom}/{y}/{x}"
  world_imagery <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{zoom}/{y}/{x}"
  world_terrain <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{zoom}/{y}/{x}"
  de_Lorme <-"https://server.arcgisonline.com/ArcGIS/rest/services/Specialty/DeLorme_World_Base_Map/MapServer/tile/{zoom}/{y}/{x}"
  world_gray <- "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{zoom}/{y}/{x}"
  world_street <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{zoom}/{y}/{x}"

  # If the baseMap param is set to a preset, set the query string to the preset
  # Otherwise default to the esri topo tile server
  if ( !is.null(baseMap) ) {
    if ( baseMap %in% AVAILABLE_PRESETS ) {
      tileserver_query_string <- eval(parse(text=baseMap))
    } else {
        stop(paste0("Required parameter baseMap = '", baseMap, "' is not a valid preset."))
    }
  } else {
    tileserver_query_string <- world_topo
  }

  # create tmp_dir
  dir.create('tmp_tiles/')

  # curl images
  images <-
    purrr::pmap(tile_grid$tiles,
                function(x, y, zoom){
                  outfile <- glue::glue("tmp_tiles/{x}_{y}.jpg")
                  curl::curl_download(url = glue::glue(tileserver_query_string),
                                      destfile = outfile)
                  outfile
                },
                zoom = tile_grid$zoom)

  # returns a RasterBrick
  raster_out <- slippymath::compose_tile_grid(tile_grid, images)

  # remove the curled tmp_tiles
  unlink('tmp_tiles', recursive=TRUE)

  return(raster_out)

}
