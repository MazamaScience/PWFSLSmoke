#' @keywords plotting
#' @export
#'
#' @title Create a rasterBrick from a tiled image server
#'
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level; corresponds to \code{ggmap::get_map()} zoom level
#' @param tileServer tile server to use, if none defaults to Esri Topo
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
#' @description Uses the input coordinates to fetch and composite a raster from a tile server. Returns a
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
#' @return A rasterBrick object which can be plotted with
#' \code{staticmap_plotRasterBrick()} or \code{raster::plotRGB()} and serve as a
#' base plot.
#'
#' @examples
#' \dontrun{
#' mapRaster <- staticmap_getTiledImagek(-122.3318, 47.668)
#' staticmap_plotRasterBrick(mapRaster)
#' }
#' @seealso \code{\link{staticmap_plotRasterBrick}}

staticmap_getTiledImage <- function(
  centerLon = NULL,
  centerLat = NULL,
  zoom = 12,
  bbox = NULL,
  tileServer = NULL,
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
  } else if ( !is.null(bbox) ) {
    bboxSR <- "4326"
  } else {
    stop("centerLat + centerLon, or bbox must be specified")
  }

  # Maximum Tiles
  max_tiles_param <- maxTiles

  # convert bbox to tile grid
  tile_grid <- slippymath::bbox_to_tile_grid(outlook_bbox, max_tiles = max_tiles_param)

  # ----- Toggle between Tile Servers -----------------------------------------------
  # Setting an Available Presets list for a user to enter, with the option to specify a
  # entirely unique url for a tile server. Examples can be found here https://leaflet-extras.github.io/leaflet-providers/preview/.

  # NOTE: parameters must be x,y and zoom explicitly! Some tile servers have zoom as 'z', and an additional {s} parameter for
  # load balancing tile requests. It is imperitive that the user specify a static server ({s}) in their custom tile server URL,
  # and ensure they switch 'z' parameters to 'zoom'.

  # NOTE: Tile Servers must return a RasterBrick object with slippymath, and not a tg_composite() (Open Street Maps)

  # Available Presets
  AVAILABLE_PRESETS <- c("esri_topo", "esri_imagery", "esri_terrain")
  esri_topo <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{zoom}/{y}/{x}"
  esri_imagery <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{zoom}/{y}/{x}"
  esri_terrain <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{zoom}/{y}/{x}"

  # If the tileServer param is set to a preset, set the query string to the preset
  # If the tileServer param is set to a url, allow the user to define a custom tileserver at their own risk
  # Otherwise default to the esri topo tile server
  if ( !is.null(tileServer) ) {
    if ( tileServer %in% AVAILABLE_PRESETS ) {
      tileserver_query_string <- eval(parse(text=tileServer))
    } else {
      if ( startsWith(tileServer, "https://") ) {
        tileserver_query_string <- tileServer
      } else {
        stop("For custom tileServer parameters, please enter a valid preset or a proper tile server URL")
      }
    }
  } else {
    tileserver_query_string <- esri_topo
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

  # returns a raster brick
  raster_out <- slippymath::compose_tile_grid(tile_grid, images)

  # remove the curled tmp_tiles
  unlink('tmp_tiles', recursive=TRUE)

  return(raster_out)

}
