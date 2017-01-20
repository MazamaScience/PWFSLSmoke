# The monitor_performanceMap() function should be renamed to monitorMap_performance() 
# and should have the following arguments:
#   predicted
# observed
# threshold=AQI$breaks_24[3]
# sizeBy=NULL
# colorBy="heidikeSkill"
# breaks=c(-Inf,.5,.6,.7,.8,Inf)
# paletteFunc=grDevices::colorRampPalette(RColorBrewer::brewer.pal(6,"Purples")[-1])
# showLegend=TRUE
# stateCol
# stateLwd
# countyCol
# countyLwd
# add=FALSE
# ...
# This function will wrap the points() function rather than the map() function and the documentation should 
# make this clear to the user. The goal is to make this function() similar to monitorMap(), to have reasonable
# defaults and to allow a clever R user to pass in additional arguments that get used by points().

#' @keywords monitor
#' @export
#' @title Create Map of Monitor Prediction Performance
#' @param predicted data list of class \code{ws_monitor}
#' @param observed data list of class \code{ws_monitor}
#' @param threshold value used to classify \code{predicted} and \code{observed} measurements
#' @param sizeBy name of the metric used to create relative sizing
#' @param colorBy name of the metric used to create relative colors
#' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks - Must also specify the colorBy paramater
#' @param paletteFunc a palette generating function as returned by \code{colorRampPalette}
#' @param showLegend logical specifying whether to add a legend (default: \code{TRUE})
#' @param stateCol color for state outlines on the map
#' @param stateLwd width for state outlines
#' @param countyCol color for county outline on the map
#' @param countyLwd width for county outlines
#' @param add logical flag that specifies whether to add to the current plot
#' @param ... additional arguments to be passed to the map() funciton such as graphical parameters (see par)
#' @description 
#' @examples 
#' \dontrun{
#' }
monitorMap_performance <- function (predicted,
                                    observed,
                                    threshold=AQI$breaks_24[3],
                                    cex=par('cex'), ### TODO
                                    sizeBy=NULL,
                                    colorBy="heidikeSkill",
                                    breaks=c(-Inf,.5,.6,.7,.8,Inf),
                                    paletteFunc=grDevices::colorRampPalette(RColorBrewer::brewer.pal(6,"Purples")[-1]),
                                    showLegend=TRUE,
                                    stateCol,
                                    stateLwd,
                                    countyCol,
                                    countyLwd,
                                    add=FALSE,
                                    ...) {
  
  #------------- below copies from monitor_performanceMap-------------------
  # Get the performance dataframe
  performanceDF <- monitor_performance(predicted, observed, threshold, threshold)
  
  # Set default graphical parameters unless they are passed in
  argsList <- list(...)
  argsList$pch <- ifelse('pch' %in% names(argsList), argsList$pch, 16)
  argsList$cex <- ifelse('cex' %in% names(argsList), argsList$cex, 1)
  # We need a default cex to use when we want to use sizeBy
  defaultCex <- argsList$cex
  
  # Plot the basemap
  longitudeRange <- range(predicted$meta$longitude)
  latitudeRange <- range(predicted$meta$latitude)
  
  # When only a single monitor is being plotted, i.e. only a single pair of coordinates is
  # supplied, plot the corresponding state
  
  if (longitudeRange[1]==longitudeRange[2] && latitudeRange[1]==latitudeRange[2]){
    
    stateCode <- suppressMessages( getStateCode(longitudeRange[1], latitudeRange[1]) )
    state.fips <- maps::state.fips
    duplicateIndex <- duplicated(state.fips$abb)
    state.fips <- state.fips[!duplicateIndex,]
    stateName <- as.character( state.fips$polyname[ which(state.fips$abb==stateCode) ] )
    stateName <- stringr::str_split_fixed(stateName, ':', 2)[1]
    maps::map('state', stateName)
    
  } else {
    
    maps::map("state",
              xlim=adjustRange(longitudeRange, 1.5, 0.5),
              ylim=adjustRange(latitudeRange, 1.5, 0.5)) 
    
  }
  # NOTE:  We should probably always show counties. No need for this to be optional.
  showCounties <- TRUE
  if (showCounties) maps::map('county',col='gray90',add=TRUE)  
  
  # Sizing
  if ( !is.null(sizeBy) && sizeBy %in% names(performanceDF)) {
    argsList$cex <- defaultCex * performanceDF[[sizeBy]] / max(performanceDF[[sizeBy]], na.rm = TRUE) 
  }
  
  # This chunk  of code figures out colors for the map.
  if ( !is.null(colorBy) && colorBy %in% names(performanceDF) ) {
    if ( length(breaks) == 1 ) {
      probs <- seq(0,1,length.out=(breaks+1))
      breaks <- stats::quantile(performanceDF[[colorBy]], probs=probs, na.rm=TRUE)
    }
    indices <- .bincode(performanceDF[[colorBy]], breaks=breaks, include.lowest=TRUE)
    colors <- paletteFunc(max(indices,na.rm=TRUE))
    argsList$col <- colors[indices]
  }
  
  # Assign the x and y arguments
  argsList$x <- predicted$meta$longitude
  argsList$y <- predicted$meta$latitude
  
  # Call the points() function
  do.call(points, argsList)
}