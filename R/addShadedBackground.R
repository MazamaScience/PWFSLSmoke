# Placeholder for now -- will update with more meat later
# 
# BACKGROUND (from TRAC) =======================
# Create a function that adds a shading to an existing plot using any variable that shares the same 
# length as the time axis of the current plot. The function signature will look like this:
#   addShadedBackground <- function(x, timeAxis, breaks=quantile(x), col='blue', maxOpacity=0.5, lwd=1)
#     Inside of the function you can create bins with:
#     assignedBin <- .bincode(x, breaks, include.lowest=TRUE)
#     You will need loop over the number of unique bins (i.e. length(breaks)-1) to create a set of colors
#     with opacity varying from 0.0 to maxOpacity.
#     Color will be added with something similar to:
#       abline(v=timeAxis, col=colors[assignedBin])
# 
# 
# addShadedBackground <- function(x, timeAxis, breaks=quantile(x), col='blue', maxOpacity=0.5, lwd=1) {
#   
# }