#' @encoding UTF-8
#' @title Camp Fire example dataset
#' @format A \emph{ws_monitor} object with "meta" and "data" dataframes.
#' @description The \code{Camp_Fire} dataset provides a quickly loadable
#' version of a \emph{ws_monitor} object for practicing and code examples.
#'
#' This dataset was was generated on 2021-11-18 by running:
#'
#' \preformatted{
#' library(PWFSLSmoke)
#'
#' Camp_Fire <-
#'   monitor_loadAnnual(2018) %>%
#'   monitor_subset(stateCodes = 'CA') %>%
#'   monitor_subset(tlim = c(20181108, 20181123))
#'
#' save(Camp_Fire, file = "data/Camp_Fire.RData")
#' }
"Camp_Fire"


#' @encoding UTF-8
#' @title Carmel Valley example dataset
#' @format A \emph{ws_monitor} object with "meta" and "data" dataframes.
#' @description
#' In August of 2016, the Soberanes fire in California burned along the Big Sur
#' coast. It was at the time the most expensive wildfire in US history. This dataset contains
#' PM2.5 monitoring data for the monitor in Carmel Valley which shows heavy smoke
#' as well as strong diurnal cycles associated with sea breezes.
#'
#' The \code{Camp_Fire} dataset provides a quickly loadable
#' version of a \emph{ws_monitor} object for practicing and code examples.
#'
#' documentation.
"Carmel_Valley"


#' @encoding UTF-8
#' @title Northwest_Megafires example dataset
#' @format A \emph{ws_monitor} object with "meta" and "data" dataframes.
#' @description
#' In the summer of 2015 Washington state had several catastrophic wildfires that led
#' to many days of heavy smoke in eastern Washington, Oregon and northern Idaho.
#' The Northwest_Megafires dataset contains AirNow ambient monitoring data for the
#' Pacific Northwest from May 31 through November 01, 2015 (UTC).
#'
#' The \code{Northwest_Megafires} dataset provides a quickly loadable
#' version of a \emph{ws_monitor} object for practicing and code examples.
#'
"Northwest_Megafires"

