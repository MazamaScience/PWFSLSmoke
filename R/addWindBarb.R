#' @keywords internal
#' @import graphics
#' @title Add wind barb to a map
#' @param x longitude
#' @param y latitude
#' @param speed wind speed in knots 
#' @param dir wind direction in degrees clockwise from north
#' @param circleSize size of the circle 
#' @param circleFill circle fill color (currently not supported)
#' @param lineCol line color (currently not supported)
#' @param extraBarbLength add length to barbs
#' @param barbSize size of the barb 
#' @param ... additional arguments to be passed to \code{lines}
#' @description Add a wind barb to the plot. Used internally in \link{addWindBarbs}
#' @references \url{https://commons.wikimedia.org/wiki/Wind_speed}

# NOTE:  This is a non-exported function so we cant have an @example
# maps::map('state', "washington")
# addWindBarb(-122, 47, speed = 125, dir = 45,
#             circleSize = 1, barbSize = 1.5, lwd = 2)

addWindBarb <- function(x, 
                        y,
                        speed,
                        dir,
                        circleSize = 1,
                        circleFill = 'transparent',
                        lineCol = 1,
                        extraBarbLength = 0,
                        barbSize = 1,
                        ...) {
  
  # Wind direction is measured in degrees clockwise from north
  # We want to convert into counter-clockwise from east
  dir <- (360 - dir + 90) %% 360
  
  # Get dir in radians
  
  rad <- dir * pi / 180
  
  # Get x and y scale factors
  pin <- par("pin")
  usr <- par("usr")
  xpi <- (usr[2]-usr[1])/pin[1]
  ypi <- (usr[4]-usr[3])/pin[2]
  
  # Add a little circle
  # Default radius is 1/24 inch. Change based on circleSize 
  rx <- xpi/24*circleSize
  ry <- ypi/24*circleSize
  theta <- seq(0, 2*pi, length = 50)
  xx <- x + rx*cos(theta)
  yy <- y + ry*sin(theta)
  
  polygon(xx, yy, col = circleFill, border = lineCol, ...)

  # Only add barb if speed > 0
  if ( speed > 0 ) {
    # The baseline barb length will be 1/4 inch
    lx <- xpi / 4 * barbSize
    ly <- ypi / 4 * barbSize
    if (speed < 5) {
      # under 5 knots, barb length is shorter with lower speeds
      lx <- lx/5*speed
      ly <- ly/5*speed
    }
    
    # Get starting and ending points for barb
    xs <- x+rx*cos(rad)
    ys <- y+rx*sin(rad)
    xe <- xs+(lx + extraBarbLength*lx)*cos(rad)
    ye <- ys+(ly + extraBarbLength*ly)*sin(rad)
    
    
    # Plot the line
    lines(c(xs, xe), c(ys, ye), col = lineCol, ...)
    if (speed >= 5) {
      # Add flags
      # flag angle 
      fa <- rad + 75*pi/180

      # 5 knots
      # start at 5/6 of the way up the barb
      # length is 1/4 of the barb length
      # position counts in by 1/6th of the barb from outside
      add_5 <- function(position) {
        fxs <- xs + (lx + extraBarbLength*lx)*cos(rad) - lx*cos(rad)*(position-1)/6
        fxe <- fxs + lx/4*cos(fa)
        fys <- ys + (ly + extraBarbLength*ly)*sin(rad) - ly*sin(rad)*(position-1)/6 
        fye <- fys + ly/4*sin(fa)
        
        
        lines(c(fxs, fxe), c(fys, fye), col=lineCol, ...)
        
      }
      

      # 10 knots
      # start at end of barb
      # length is 1/2 of barb length
      # position counts in by 1/6th of the barb from outside
      add_10 <- function(position) {
        fxs <- xs + (lx + extraBarbLength*lx)*cos(rad) - lx*cos(rad)*(position-1)/6
        fxe <- fxs + lx/2*cos(fa)
        fys <- ys + (ly + extraBarbLength*ly)*sin(rad) - ly*sin(rad)*(position-1)/6 
        fye <- fys + ly/2*sin(fa)
        
        
        lines(c(fxs, fxe), c(fys, fye), col = lineCol,  ...)
        
      }
      
      # 50 knots
      add_50 <- function(position) {
        fx1 <- xs + (lx + extraBarbLength*lx)*cos(rad) - lx*cos(rad)*(position)/6
        fx2 <- fx1 + lx/2*cos(fa)
        fx3 <- xs + (lx + extraBarbLength*lx)*cos(rad) - lx*cos(rad)*(position-1)/6
        fy1 <- ys + (ly + extraBarbLength*ly)*sin(rad) - ly*sin(rad)*(position)/6 
        fy2 <- fy1 + ly/2*sin(fa)
        fy3 <- ys + (ly + extraBarbLength*ly)*sin(rad) - ly*sin(rad)*(position-1)/6 
        
        polygon(c(fx1, fx2, fx3, fx1), c(fy1, fy2, fy3, fy1), col = lineCol, border = lineCol, ...) 
        
      }
      
      
      fifties <- speed %/% 50
      tens <- (speed %% 50) %/% 10
      fives <- (speed %% 10) %/% 5
      
      if (fifties > 0) {
        
        for (i in 1:fifties) {
          add_50(i)
        }
        if (tens > 0) {
          for (i in (fifties+1):(fifties+1+tens)) {
            add_10(i)
          }
        }
        if (fives > 0) {
          add_5(fifties + tens + 2)
        }
      } else {
        
        if (tens > 0) {
          for (i in 1:tens) {
            add_10(i)
          }
        }
        
        
        if (fives == 1) {
          add_5(max(c(tens, 1))+1)
        }
        
      }
      
    }
  }
  
  

  
  
}
