# get the colors for for individuals in a population based on either start
# dates or synchrony values. For now synchrony is standard augspurger synchrony
get_point_colors <- function(popn, startOrSync = "start", syncMethod = "aug") {
  popfr <- popn$df
  if (startOrSync == "start") {
    # get first, last, (and peak)
    firstDay <- min(popfr[[popn$start]])
    lastDay <- max(popfr[[popn$start]])
    peakDay <- median(popfr[[popn$start]])
    # get a scale factor
    dayAfterFirst <- as.numeric(popfr[[popn$start]] - firstDay)
    totalDays <- as.numeric(lastDay - firstDay)
    scales <- dayAfterFirst/totalDays
    # get red and blue values
    rb <- scales*510
    r <- ifelse(rb < 255, rb, 255)
    b <- ifelse(rb < 255, 255, 510-rb)
    rgbValues <- rgb(red = r, green = 0, blue = b, maxColorValue = 255)

  } else if (startOrSync == "sync") {
    syncs <- synchrony(popn, method = syncMethod, synchronyType = "ind")
    rb <- syncs$synchrony*510
    r <- ifelse(rb < 255, rb, 255)
    b <- ifelse(rb < 255, 255, 510-rb)
    rgbValues <- rgb(red = r, green = 0, blue = b, maxColorValue = 255)
  } else {
    stop("startOrSync can only be \"start\" or \"sync\"")
  }
  return(rgbValues)
}

##' Create a map of the population where the colors of each point represents
##' either the start date or the synchrony value of the individual
##'
##' @title Map the population with temporal information
##' @param popn a 3D population object
##' @param startOrSync must be either "start" or "sync". Determines whether
##' to get colors from start dates or synchrony values
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' mapWithColors(pop)
mapWithColors <- function(popn, startOrSync = "start") {
  rgbVals <- get_point_colors(popn, startOrSync)
  popfr <- popn$df
  plot(popfr[[popn$x]], popfr[[popn$y]], col = rgbVals, pch = 16,
       xlab = popn$x, ylab = popn$y, asp = 1)
  leg <- c("early", "peak", "late")
  if (startOrSync == "sync") leg <- c("low", "middle", "high")
  legend("topleft", legend = leg, pch = c(16,16,16),
         col = c(rgb(0,0,1), rgb(1,0,1),  rgb(1,0,0)))
}

##' Add lines to show the positions of the earliest and latest flowering
##' individuals within a population to a map
##'
##' @title Find earliest and latest flowering
##' @param popn a 3D population object
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' mapWithColors(pop)
##' pinpointExtremes(pop)
pinpointExtremes <- function(popn) {
  first <- popn$df[which.min(popn$df[[popn$start]]),]
  last <- popn$df[which.max(popn$df[[popn$start]]),]
  abline(v = first[[popn$x]], col = "blue")
  abline(h = first[[popn$y]], col = "blue")
  abline(v = last[[popn$x]], col = "red")
  abline(h = last[[popn$y]], col = "red")
}

##' Creates a map that includes all individuals that were flowering on
##' a given day or set of days
##'
##' @title Map flowering individuals on a given day
##' @param popn a 3D population object
##' @param day integer of which flowering day to plot
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' mapForDay(pop, 8)
mapForDay <- function(popn, day) {
  indDaily <- individualDailyFlowering(popn)
  if (length(day) > 1) {
    popnOnDay <- popn$df[rowSums(indDaily[,day]) > 0,]
  } else {
    popnOnDay <- popn$df[indDaily[,day],]
  }
  xax <- c(min(popn$df[[popn$x]]), max(popn$df[[popn$x]]))
  yax <- c(min(popn$df[[popn$y]]), max(popn$df[[popn$y]]))
  plot(popnOnDay[[popn$x]],popnOnDay[[popn$y]], xlim = xax, ylim = yax,
       xlab = popn$x, ylab = popn$y, asp = 1, pch = 16, cex = 0.5,
       col = rgb(60,141,13, maxColorValue = 255)) # Christmas green
}

##' plotMap makes a map of a population
##'
##' @title graphical visualization of a spatial disribution
##' @param pp dataframe in 3dpop format, at least with x and y columns
##' @param xlab character label for the x-axis, defaults to "E"
##' @param ylab character label for the y-axis, defaults to "N"
##' @param pch optional plotting character
##' @param ... graphical parameters may also be supplied as arguments
##' @return nothing
##' @return optional arguments for the plot function
##' @export
##' @author Stuart Wagenius
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' plotMap(pop)
##' \dontrun{plotMap(NULL)}
plotMap <- function (pp, xlab = "E", ylab = "N", pch = NULL, ...)
{
  plot.default(pp$df[, pp$x], pp$df[, pp$y], type = "n", xlab = xlab,
               ylab = ylab, ...)
  if(is.null(pch)) {
    text(pp$df[, pp$x], pp$df[, pp$y], pp$df[, pp$id], ...)
  } else {
    points(pp$df[, pp$x], pp$df[, pp$y], pch = pch, ...)
  }
}

