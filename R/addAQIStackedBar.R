#' @title Create Stacked AQI Bar
#' @param width width of the bar as a fraction of the width of the plot area (default = .02)
#' @param height height of the bar as a fraction of the height of the plot area (default = 1)
#' @param labels logical indicating whether to draw labels for AQI levels
#' @param pos position of the stacked bar. Either 'left' or 'right'
#' @param title logical indicating whether to include a title for the bar
#' @param cex.title character expansion for the title
#' @param srt.title string rotation in degrees for the title
#' @description Draws a stacked bar indicating AQI levels on one side of a plot
#' @return Stacked AQI Bar

addAQIStackedBar <- function(width = .02,
                             height = 1,
                             labels = TRUE,
                             pos = "right",
                             title = TRUE,
                             cex.title = 1.2,
                             srt.title = 0) {

  # TODO:  remove 'labels' and 'title' arguments?
  
  usr <- par("usr")
  
  if (pos == "right") {
    l <- usr[2]-.01*(usr[2]-usr[1])
    r <- l+(width)*(usr[2]-usr[1])
  } else if (pos == "left") {
    l = usr[1]
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
  
  if ( title ) {
    text(mean(l,r), height*usr[4], labels = "AQI", xpd = NA, font = 2, cex = cex.title, pos = 3, srt = srt.title)
  }
  
  if ( labels ) {
    x <- usr[2]
    ylo <- c(0,AQI$breaks[2:6])
    yhi <- c(AQI$breaks_24[2:6],350.5)
    y <- ylo + (yhi-ylo)/2
    names <- c("G", "M", "USG", "U", "VU", "H")
    text(x, y, names, xpd = NA, pos = 4)
  }
  
}