##' Create a visualization of the mating schedule of a population
##'
##' @title Graphical visualization of a flowering schedule
##' @param fs, a data frame in flowering schedule format
##' @param opening optional date to specify first date of interest
##' @param closing optional date to specify last date of interest
##' @param xlab, label for the x-axis, defaults to "date"
##' @param ylab, label for the y-axis, defaults is blank
##' @param ... graphical parameters may also be supplied as arguments
##' @return nothing
##' @export
##' @author Stuart Wagenius
##' @examples
##' fs <- generateFS()
##' plotFS(fs)
##' \dontrun{plotFS(NULL)}
plotFS <- function(fs, opening = NULL, closing = NULL, xlab = "date",
                   ylab = "", ...) {
  if (is.null(opening))
    opening <- min(fs$df[, fs$start])
  if (is.null(closing))
    closing <- max(fs$df[, fs$end])
  count <- dim(fs$df)[1]
  fs$df <- fs$df[order(fs$df[, fs$start], fs$df[, fs$end]), ]
  fs$df$index <- seq_along(fs$df[, 1])
  plot.default(fs$df[, fs$start], fs$df$index, ylim = c(1, count),
               xlim = c(opening, closing), type = "n", xlab = xlab,
               ylab = ylab, xaxt = "n", ...) # yaxt = "n",
  datLabs <- seq(opening, closing, by = 7)
  axis(1, at = datLabs,
       labels = format(as.Date(datLabs,origin = as.Date("1970-01-01")),
                       format = "%b %d"))
  segments(fs$df[, fs$start], fs$df$index, fs$df[, fs$end],
           fs$df$index, col = "gray50", ...)
}

##' pointsFS adds points to a flowering schedule figure
##'
##' A great function. It invisibly returns the same thing as summaryFS.
##'
##' @title add daily summary of flowering schedule to a plot
##' @param fs a list in flowering schedule or 3dpop format
##' @param peak logical add vertical line at peak date, default is false
##' @param ... graphical parameters may also be supplied as arguments
##' @return the output of summaryFS invisibly
##' @export
##' @author Stuart Wagenius
##' @seealso see functions \code{\link{plotFS}} and \code{\link{summaryFS}}
##' @examples
##' pop <- simulateScene()
##' plotFS(pop)
##' pointsFS(pop, type = "b")
pointsFS <- function(fs, peak = FALSE, ...) {
  sfs <- summaryFS(fs)
  points(sfs$fl.density$day, sfs$fl.density$count, ...)
  if(peak) abline(v = sfs$peakDate, ...)
  invisible(sfs)
}

##' A great function
##'
##' This function reads the output of the bootstrapFS function
##'
##' @param bs dataframe output from bootstrapFS
##' @param peak logical indicating whether to plot confidence interval for peak date.
##' @param opening logical indicating whether to plot confidence interval for opening date.
##' @param closing logical indicating whether to plot confidence interval for closing date.
##' @param medianFD logical indicating whether to plot confidence interval for median first date.
##' @param medianLD logical indicating whether to plot confidence interval for median last date.
##' @param lwd numeric graphical parameter for width of CI line segment
##' @param code numeric graphical parameter for arrows at end of CI line segment
##' @param length numeric graphical parameter for length of arrow as end of CI line segment
##' @param ... Further graphical parameters may also be supplied as arguments.
##'
##' @export
##' @author Stuart Wagenius
##' @examples
##' \dontrun{addCIs(NULL)}
##'
addCIs <- function(bs, peak = TRUE, opening = TRUE, closing = TRUE, medianFD = TRUE, medianLD = TRUE,
                   lwd = 2, code = 3, length = 0.1, ...){
  if(dim(bs$CI)[1] != 2)
    message("The first two of these are used for the CI: ", row.names(bs$CI))
  zli <- bs$CI[1, ] == bs$CI[2, ]
  msg <- "A confidence interval had length zero and was skipped"
  if(any(zli)){
    if(zli[1]) {peak <- FALSE; message(msg)}
    if(zli[2]) {opening <- FALSE; message(msg)}
    if(zli[3]) {closing <- FALSE; message(msg)}
    if(zli[4]) {medianFD <- FALSE; message(msg)}
    if(zli[5]) {medianLD <- FALSE; message(msg)}
  }
  if(peak)     arrows(bs$CI[1, "peakDate"], bs$n, bs$CI[2, "peakDate"], bs$n,
                      lwd = lwd, code = code, length = length, ...)
  if(opening)  arrows(bs$CI[1, "firstDay"], 1, bs$CI[2, "firstDay"], 1,
                      lwd = lwd, code = code, length = length, ...)
  if(closing)  arrows(bs$CI[1, "lastDay"], 1, bs$CI[2, "lastDay"], 1,
                      lwd = lwd, code = code, length = length, ...)
  if(medianFD) arrows(bs$CI[1, "range50.1"], bs$n/2, bs$CI[2, "range50.1"], bs$n/2,
                      lwd = lwd, code = code, length = length, ...)
  if(medianLD) arrows(bs$CI[1, "range50.2"], bs$n/2, bs$CI[2, "range50.2"], bs$n/2,
                      lwd = lwd, code = code, length = length, ...)
}

##' add vertical lines to a flowering schedule plot
##'
##' A great function
##'
##' This function reads draws lines in desirable places
##'
##' @param fs a flowering schedule or 3dpop object
##' @param peak logical indicating whether to draw line at the peak date.
##' @param medianFD logical indicating whether to draw a line at the median first date.
##' @param medianLD logical indicating whether to draw a line at the median last date.
##' @param ... Further graphical parameters may also be supplied as arguments.
##'
##' @export
##' @author Stuart Wagenius
##' @examples
##' \dontrun{vlinesFS(NULL)}
##'
vlinesFS <- function(fs, peak = TRUE, medianFD = TRUE, medianLD = TRUE, ...){
  sfs <- summaryFS(fs)
  df.size <- dim(fs$df)[1]
  if(peak) abline(v = sfs$peakDate, ...)
  if(medianFD) segments(sfs$range50[1], -1, sfs$range50[1], df.size/2, ...)
  if(medianLD) segments(sfs$range50[2], -1, sfs$range50[2], df.size/2, ...)
}
