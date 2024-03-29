% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/staticmap_getRasterBrick.R
\name{staticmap_getRasterBrick}
\alias{staticmap_getRasterBrick}
\title{Create a rasterBrick from a tiled image server}
\usage{
staticmap_getRasterBrick(
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
)
}
\arguments{
\item{centerLon}{Map center longitude.}

\item{centerLat}{Map center latitude.}

\item{maptype}{Defaults to \href{http://www.arcgis.com/home/item.html?id=30e5fe3149c34df1ba922e6f5bbf808f}{Esri Topographic}
Available to select between Stamen basemaps or Esri basemaps.\cr
\strong{Stamen}\cr
\itemize{
  \item terrain
  \item terrain-background
  \item terrain-labels
  \item terrain-lines
  \item toner
  \item toner-background
  \item toner-hybrid
  \item toner-labels
  \item toner-labels
  \item toner-lines
  \item toner-lite
  \item watercolor
}
\strong{Esri}\cr
\itemize{
  \item "world_topo"
  \item "world_imagery"
  \item "world_terrain"
  \item "de_Lorme"
  \item "world_grey"
  \item "world_streets"
}}

\item{zoom}{Map zoom level.}

\item{width}{Width of image, in pixels.}

\item{height}{Height of image, in pixels.}

\item{bbox}{If you are using the Esri maps, then the \code{bbox} parameter
must be an \code{st_bbox} object as specificed in the \code{sf} package documentation
\url{https://www.rdocumentation.org/packages/sf/versions/0.7-4/topics/st_bbox}.
If using Stamen Maps, use a vector organized as (lonLo, latLo, lonHi, latHi)
If not null,
\code{centerLon}, \code{centerLat}, and \code{zoom} are ignored.}

\item{maxTiles}{Only utilized if selecting an esri basemap, specifies the
maximum number of tiles to be returned. The greater the number,
the slower the performance -- arbitrarily set to 20 by default.}

\item{crs}{Object of class CRS. The Coordinate Reference System (CRS) for the
returned map. If the CRS of the downloaded map does not match, it will be
projected to the specified CRS using \code{raster::projectRaster}.}

\item{tileCacheDir}{Optional location for cached tiles.}
}
\value{
A rasterBrick object which can be plotted with
\code{staticmap_plotRasterBrick()} or \code{raster::plotRGB()} and serve as a
base plot.
}
\description{
Uses the input coordinates to select an appropriate method to
build a \code{raster::rasterBrick} object. It will either use the
\code{staticmap_getStamenmapBrick()} function or the
\code{staticmap_getEsrimapBrick()} function This can then passed as the
\code{rasterBrick} object to the \code{staticmap_plotRasterBrick()} function
for plotting.
}
\note{
The spatial reference of the image when it is downloaded is 3857. If
the crs argument is different, projecting may cause the size and extent of
the image to differ very slightly from the input, on a scale of 1-2 pixels or
10^-3 degrees.

If bbox is specified and the bbox aspect ratio does not match the
width/height aspect ratio the extent is resized to prevent the map image from
appearing stretched, so the map extent may not match the bbox argument
exactly.
}
\examples{
\dontrun{
# Fail gracefully if any resources are not available
try({

rasterBrick <- staticmap_getRasterBrick(-122.3318, 47.668)
staticmap_plotRasterBrick(rasterBrick)


rasterBrick <- staticmap_getRasterBrick(-122.3318, 47.668, "world_streets", 12)
staticmap_plotRasterBrick(rasterBrick)


rasterBrick <- staticmap_getRasterBrick(-122.3318, 47.668, "watercolor", 12)
staticmap_plotRasterBrick(rasterBrick)

}, silent = FALSE)
}
}
\seealso{
\code{\link{staticmap_getStamenmapBrick}}

\code{\link{staticmap_getEsrimapBrick}}

\code{\link{staticmap_plotRasterBrick}}
}
\keyword{plotting}
