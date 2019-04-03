#' @export
#' @import MazamaCoreUtils
#'
#' @title Initialize Mazama Spatial Utils
#'
#' @param spatialDataDir directory where spatial datasets are created
#' @param stateCodeDataset MazamaSpatialUtils dataset returning ISO 3166-2 alpha-2 stateCodes
#' @param logLevel directory where spatial datasets are created
#' @description Convenience function that wraps:
#'
#' \preformatted{
#'   logger.setup()
#'   logger.setLevel(WARN)
#'   setSpatialDataDir('~/Data/Spatial')
#'   loadSpatialData('NaturalEarthAdm1')
#' }
#'
#' If file logging is desired, these commands should be run individually with
#' output log files specified as arguments to \code{logger.setup()}.
#' @seealso \code{\{link{logger.setup}}

initializeMazamaSpatialUtils <- function(
  spatialDataDir = '~/Data/Spatial',
  stateCodeDataset = 'NaturalEarthAdm1',
  logLevel = WARN
) {

  logger.setup()
  logger.setLevel(logLevel)
  setSpatialDataDir(spatialDataDir)
  loadSpatialData(stateCodeDataset)

}
