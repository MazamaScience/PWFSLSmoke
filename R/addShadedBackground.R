# # Placeholder for now -- will update with more meat later
# # 
# # BACKGROUND (from TRAC) =======================
# # Create a function that adds a shading to an existing plot using any variable that shares the same
# # length as the time axis of the current plot. The function signature will look like this:
# #   addShadedBackground <- function(x, timeAxis, breaks=quantile(x), col='blue', maxOpacity=0.5, lwd=1)
# #     Inside of the function you can create bins with:
# #     assignedBin <- .bincode(x, breaks, include.lowest=TRUE)
# #     You will need loop over the number of unique bins (i.e. length(breaks)-1) to create a set of colors
# #     with opacity varying from 0.0 to maxOpacity.
# #     Color will be added with something similar to:
# #       abline(v=timeAxis, col=colors[assignedBin])
# 
# addShadedBackground <- function(param, timeAxis, breaks=quantile(param), col='blue', maxOpacity=0.5, lwd=1) {
# 
#   assignedBin <- .bincode(param, breaks, include.lowest=TRUE)
#   colors <- c()  
#   for (i in 1:length(breaks)-1) { colors[i] <- adjustcolor(col,(i-1)/(length(breaks)-1)) }
#   
#   abline(v=timeAxis, col=colors[assignedBin])
#   
# }
# 
# # 
# # # FROM DNR_UTILS.R
# # 
# # DNR_stoveWindPlot <- function(ws_monitor, raw,
# #                               tempVar='AT', humidityVar='RHx', windVar='W.S',
# #                               title='', tlim=NULL) {
# # 
# #   # Flusing Winds
# #   windBin <- .bincode(raw$W.S, c(-Inf,1,2,5,10,Inf))
# #   for (i in 5) { colors[i] <- adjustcolor('blue',(i-1)/5) }
# # 
# #   # Create fancy woodburning index
# #   stoveIndex <- woodStoveIndex(raw, tempVar, humidityVar, rollingWidth=12, rollingAlign='left')
# #   stoveSmoke <- ws_monitor$data[,2]
# #   stoveSmoke[stoveIndex < 0.5] <- NA
# # 
# #   monitor_timeseriesPlot(ws_monitor, type='l', shadedNight=FALSE)
# #   points(stoveSmoke ~ ws_monitor$data$datetime, pch=16, col='purple')
# #   abline(v=raw$datetime, col=colors[windBin])
# #   legend('topleft', legend=c("Woodstove Index", "0-1 m/s Wind", "1-2 m/s Wind", "2-5 m/s Wind"), fill=c('purple',colors[1],colors[2],colors[3]))
# # 
# # }