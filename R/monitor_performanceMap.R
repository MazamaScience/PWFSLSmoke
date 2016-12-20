#' @keywords monitor
#' @export
#' @title Create Map of Monitor Prediction Performance
#' @param predicted data list of class \code{ws_monitor}
#' @param observed data list of class \code{ws_monitor}
#' @param threshold value used to classify \code{predicted} and \code{observed} measurements
#' @param sizeBy name of the metric used to create relative sizing
#' @param colorBy name of the metric used to create relative colors
#' @param paletteFunc a palette generating function as returned by \code{colorRampPalette}
#' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks - Must also specify the colorBy paramater
#' @param ... additional arguments to be passed to the plot() funciton such as graphical parameters (see par)
#' @description This function uses "confusion matrix" analysis to calculate
#' different measures of predictive performance for every timeseries found
#' in \code{predicted} with respect to the observed values found in the single timeseries
#' found in \code{observed}.
#' 
#' Using a single number for the \code{breaks} argument will cause the algorithm to use
#' quantiles to determine breaks.
#' @details Setting either \code{sizeBy} or \code{colorBy} to \code{NULL} will cause
#' the size/colors to remain constant.
#' @return Dataframe of monitors vs named measure of performance.
#' @seealso \link{monitor_performance}
#' @examples 
#' \dontrun{
#' # Spokane summer of 2015
#' airnow <- airnow_load(20150701,20150930)
#' airnow <- monitor_rollingMean(airnow, width=3)
#' WA <- monitor_subset(airnow, stateCode='WA')
#' MonroeSt <- monitor_subset(WA, monitorIDs=530630047)
#' monitor_performanceMap(WA, MonroeSt, cex=2)
#' title('Heidike Skill of monitors predicting another monitor.')
#' }

monitor_performanceMap <- function(predicted, observed,
                                   threshold=AQI$breaks_24[3],
                                   sizeBy=NULL, colorBy="heidikeSkill",
                                   paletteFunc=colorRampPalette(RColorBrewer::brewer.pal(6,"Purples")[-1]),
                                   breaks=c(-Inf,.5,.6,.7,.8,.9),
                                   ...) {

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
