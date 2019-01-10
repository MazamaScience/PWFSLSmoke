#' @keywords plotting
#' @export
#' @title Create Stacked AQI Bar
#' @param width width of the bar as a fraction of the width of the plot area (default = .02)
#' @param height height of the bar as a fraction of the height of the plot area (default = 1)
#' @param pos position of the stacked bar. Either 'left' or 'right'
#' @description Draws a stacked bar indicating AQI levels on one side of a plot
#' @return Stacked AQI Bar

addAQIStackedBar <- function(width = .01,
                             height = 1,
                             pos = "left") {

  # TODO:  remove 'labels' and 'title' arguments?

  usr <- par("usr")

  if (pos == "right") {
    l <- usr[2]-width*(usr[2]-usr[1])
    r <- usr[2]
  } else if (pos == "left") {
    l <- usr[1]
    r <- usr[1]+width*(usr[2]-usr[1])
  }

  for (i in 1:6) {
    rect(l,
         min(max(0,AQI$breaks_24[i]), height*usr[4]),
         r,
         min(AQI$breaks_24[i+1], height*usr[4]),
         col = AQI$colors[i],
         xpd = NA,
         border = NA
    )
  }

}
