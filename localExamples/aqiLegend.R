# #
# # # arguments
# # rowScaleFactors <- c(1,1,1,1,1,1,1)
# # columnScaleFactors <- c(1,1,1)
# # wordWrapWidth <- c(17,1,62)
# # opac_aqi <- 100 # expressed as percent; anything below 50% is adjusted up to 50%
# # fontScalar <- 1
# #
# # col1Wrap <- rep(17,7)
# # col2Wrap <- rep(1,7)
# # col3Wrap <- rep(62,7)
# #
# # colWrap <- data.frame(col1Wrap,col2Wrap,col3Wrap)
# #
# # # FUNCTION ---------------------------------
# #
# # #aqiLegend_complex <- function(rowScaleFactors=rep(1,7),columnScaleFactors=rep(1,3),wordWrapWidth=c(3,1,6),opac_aqi=100,fontScalar=1) {
# #
# #   # ----- Style ---------------------------------
# #
# #   # Colors
# #   opac_aqi <- max(.5,opac_aqi/100)
# #   col_aqi <- adjustcolor(AQI$colors[1:6],opac_aqi)
# #   col_header <- 'gray75'
# #   col_cells <- c(col_header,col_aqi)
# #
# #   # Grid and border
# #   lwd_grid <- 2
# #   lwd_border <- 4
# #   col_grid <- 'black'
# #   col_border <- 'black'
# #
# #   # Default row heights and column widths
# #   rowHeights <- c(2,2,2,2,2,2,2)
# #   columnWidths <- c(4,4,10)
# #
# #   # Increase text size for headers
# #   fontScalar <- fontScalar*1.25
# #
# #   # ----- Table text ----------------------------
# #
# #   levels <- c('Air Quality Index',
# #               'Good',
# #               'Moderate',
# #               'Unhealthy For Sensitive Groups',
# #               'Unhealthy',
# #               'Very Unhealthy',
# #               'Hazardous')
# #
# #   values <- c('Numerical Value',
# #               '0-50',
# #               '51-100',
# #               '101-150',
# #               '151-200',
# #               '201-300',
# #               '301-500')
# #
# #   # AQI Descriptions (from https://airnow.gov/index.cfm?action=aqibasics.aqi)
# #   descriptions <- c('Meaning',
# #                     'Air quality is considered satisfactory,
# #                     and air pollution poses little or no risk.',
# #                     'Air quality is acceptable; however, for some
# #                     pollutants there may be a moderate health
# #                     concern for a very small number of people who
# #                     are unusually sensitive to air pollution.',
# #                     'Members of sensitive groups may experience health effects. The general public is not likely to be affected.',
# #                     'Everyone may begin to experience health effects; members of sensitive groups may experience more serious health effects.',
# #                     'Health alert: everyone may experience more serious health effects.',
# #                     'Health warnings of emergency conditions. The entire population is more likely to be affected.')
# #
# #   tableText <- data.frame(levels,values,descriptions)
# #
# #   apply(tableText,2,nchar)[,3]
# #   maxCharCount <- apply(apply(tableText,2,nchar),1,max)
# #   maxCharCount/2
# #
# #   # ----- Data Preparation ---------------------
# #
# #   # Adjust row heights and column widths
# #   rowHeights <- rowHeights*rowScaleFactors
# #   columnWidths <- columnWidths*columnScaleFactors
# #
# #   # Calculate total plot height and width
# #   height <- sum(rowHeights)
# #   width <- sum(columnWidths)
# #
# #   # Row and column counts
# #   columnCount <- length(columnScaleFactors)
# #   rowCount <- length(rowScaleFactors)
# #
# #
# #   # ----- Plotting ------------------------
# #
# #   # Save existing graphical parameters before adjusting to remove margins
# #   oldpar <- par
# #   par(mar=c(0,0,0,0))
# #
# #   # Create blank plot
# #   plot(0,0,col='white',xlim=c(0,width),ylim=c(0,height),xlab='',ylab='',axes=FALSE)
# #
# #   # TODO: get rid of outside borders as visible using the following line
# #   #abline(v=c(0,width*1),h=c(0,height*1)); box() # temp bound lines -- see #borders below for final ones
# #
# #   # Set up dataframes to hold coordinates
# #   v <- data.frame() # vertical coordinates
# #   h <- data.frame() # horizontal coordinates
# #
# #   # Draw colored polygons and place text
# #   for (i in 1:rowCount) {
# #
# #     # determine vertical limits of each polygon
# #     v[1,i] <- height-sum(rowHeights[1:i-1])
# #     v[3,i] <- height-sum(rowHeights[1:i])
# #     v[2,i] <- sum(v[1,i],v[3,i])/2
# #
# #     # apply background color
# #     polygon(x=c(0,width,width,0),y=c(v[1,i],v[1,i],v[3,i],v[3,i]),col=col_cells[i],border=NA)
# #
# #     # set text size back to original
# #     if ( i==2 ) fontScalar <- fontScalar*0.8
# #
# #     # set text color
# #     if ( i>3 && opac_aqi>0.7 ) col_text <- 'white' else col_text <- 'black'
# #
# #     # determine horizontal limits of each cell and print text
# #     for (j in 1:columnCount) {
# #
# #       h[1,j] <- sum(columnWidths[1:j-1])
# #       h[3,j] <- sum(columnWidths[1:j])
# #       h[2,j] <- sum(h[1,j],h[3,j])/2
# #
# #
# #       #TODO: Play with witdh in following line, which appears to correspond to number of characters!
# #       #TODO: May be able to set up width based on number of lines (divide length by desired # of lines)
# #       #cellText <- paste(strwrap(tableText[i,j],width = wordWrapWidth[j]*columnWidths[j],simplify = TRUE),collapse = "\n")
# #
# #       cellText <- paste(strwrap(tableText[i,j],width = wordWrapWidth[j],simplify = TRUE),collapse = "\n")
# #       text(x=h[2,j],y=v[2,i],labels=cellText,col=col_text,cex = fontScalar)
# #
# #     }
# #
# #   }
# #
# #   # Draw grid
# #   for (i in 2:rowCount) { # horizontal grid lines
# #     lines(x=c(0,width),y=c(v[1,i],v[1,i]),lwd=lwd_grid,lend=1,col=col_grid)
# #   }
# #   for (j in 2:columnCount) { # vertical grid lines
# #     lines(x=c(h[1,j],h[1,j]),y=c(0,height),lwd=lwd_grid,lend=1,col=col_grid)
# #   }
# #
# #   # Draw border
# #   lines(x=c(0,0),y=c(0,height),lwd=lwd_border,col=col_border) # left
# #   lines(x=c(width,width),y=c(0,height),lwd=lwd_border,col=col_border) # right
# #   lines(x=c(0,width),y=c(0,0),lwd=lwd_border,col=col_border) # bottom
# #   lines(x=c(0,width),y=c(height,height),lwd=lwd_border,col=col_border) # top
# #
# #   # restore graphical parameters
# #   par <- oldpar
# #
# # #}
# #
# 
# 
# # # arguments
# # rowHeights <- c(1,1,1,1,1,1,1)
# # columnWidth <- 4
# # wordWrapWidth <- rep(17,7)
# # opac_aqi <- 100 # expressed as percent; anything below 50% is adjusted up to 50%
# # fontScalar <- 1
# 
# aqiLegend_simple <- function(wordWrapWidth=rep(17,7),rowHeights=rep(1,7),columnWidth=4,opac_aqi=100,fontScalar=1) {
# 
#   # ----- Style ---------------------------------
# 
#   # Colors
#   opac_aqi <- max(.5,opac_aqi/100)
#   col_aqi <- adjustcolor(AQI$colors[1:6],opac_aqi)
#   col_header <- 'gray75'
#   col_cells <- c(col_header,col_aqi)
# 
#   # Grid and border
#   lwd_grid <- 2
#   lwd_border <- 4
#   col_grid <- 'black'
#   col_border <- 'black'
# 
#   # Increase text size for headers
#   fontScalar <- fontScalar*1.25
# 
#   # ----- Table text ----------------------------
# 
#   levels <- c('Air Quality Index',
#               'Good',
#               'Moderate',
#               'Unhealthy For Sensitive Groups',
#               'Unhealthy',
#               'Very Unhealthy',
#               'Hazardous')
# 
#     # ----- Data Preparation ---------------------
# 
#     # Calculate total plot height and width
#     height <- sum(rowHeights)
#     width <- sum(columnWidth)
# 
#     # Row and column counts
#     rowCount <- length(rowHeights)
#     columnCount <- 1
# 
# 
#       # ----- Plotting ------------------------
# 
#       # Save existing graphical parameters before adjusting to remove margins
#       oldpar <- par
#       par(mar=c(0,0,0,0))
# 
#       # Create blank plot
#       plot(0,0,col='white',xlim=c(0,width),ylim=c(0,height),xlab='',ylab='',axes=FALSE)
# 
#       # TODO: get rid of outside borders as visible using the following line
#       #abline(v=c(0,width*1),h=c(0,height*1)); box() # temp bound lines -- see #borders below for final ones
# 
# 
# 
#         # Set up dataframes to hold coordinates
#         v <- data.frame() # vertical coordinates
#         h <- data.frame() # horizontal coordinates
# 
#         # Draw colored polygons and place text
#         for (i in 1:rowCount) {
# 
#           # determine vertical limits of each polygon
#           v[1,i] <- height-sum(rowHeights[1:i-1])
#           v[3,i] <- height-sum(rowHeights[1:i])
#           v[2,i] <- sum(v[1,i],v[3,i])/2
# 
#           # apply background color
#           polygon(x=c(0,width,width,0),y=c(v[1,i],v[1,i],v[3,i],v[3,i]),col=col_cells[i],border=NA)
# 
#           # set text size back to original
#           if ( i==2 ) fontScalar <- fontScalar*0.8
# 
#           # set text color
#           if ( i>3 && opac_aqi>0.7 ) col_text <- 'white' else col_text <- 'black'
# 
#           # determine horizontal limits of each cell and print text
#           for (j in 1:columnCount) {
# 
#             h[1,j] <- sum(columnWidth[1:j-1])
#             h[3,j] <- sum(columnWidth[1:j])
#             h[2,j] <- sum(h[1,j],h[3,j])/2
# 
# 
#             #TODO: Play with witdh in following line, which appears to correspond to number of characters!
#             #TODO: May be able to set up width based on number of lines (divide length by desired # of lines)
#             #cellText <- paste(strwrap(tableText[i,j],width = wordWrapWidth[j]*columnWidths[j],simplify = TRUE),collapse = "\n")
# 
#             cellText <- paste(strwrap(levels[i],width = wordWrapWidth[j],simplify = TRUE),collapse = "\n")
#             text(x=h[2,j],y=v[2,i],labels=cellText,col=col_text,cex = fontScalar)
# 
#           }
# 
#         }
# 
#         # Draw grid
#         for (i in 2:rowCount) { # horizontal grid lines
#           lines(x=c(0,width),y=c(v[1,i],v[1,i]),lwd=lwd_grid,lend=1,col=col_grid)
#         }
#         #for (j in 2:columnCount) { # vertical grid lines
#         #  lines(x=c(h[1,j],h[1,j]),y=c(0,height),lwd=lwd_grid,lend=1,col=col_grid)
#         #}
# 
#         # Draw border
#         lines(x=c(0,0),y=c(0,height),lwd=lwd_border,col=col_border) # left
#         lines(x=c(width,width),y=c(0,height),lwd=lwd_border,col=col_border) # right
#         lines(x=c(0,width),y=c(0,0),lwd=lwd_border,col=col_border) # bottom
#         lines(x=c(0,width),y=c(height,height),lwd=lwd_border,col=col_border) # top
# 
#         # restore graphical parameters
#       par <- oldpar
# 
# }