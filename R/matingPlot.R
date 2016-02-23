#### function to make mating scene plots ####

##' Visualize a mating scene
##'
##' @title graphical visualization of a mating scene object
##' @param popn
##' @param dimension
##' @param opening
##' @param closing
##' @param dailyPoints
##' @param drawQuartiles
##' @param includeID
##' @param sub
##' @param xlab
##' @param ylab
##' @param pch
##' @param quartileWt
##' @param quartileColor
##' @param peakColor
##' @param ...
##' @return nothing
##' @return optional arguments for the plot function
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' matingPlot(pop)
##' \dontrun{plotMap(NULL)}
##'
##'
matingPlot <- function(popn, dimension = "auto",
                       opening = NULL, closing = NULL,
                       dailyPoints = TRUE, drawQuartiles = TRUE,
                       includeID = FALSE,
                       sub= NULL, xlab = 'xlab', ylab = 'ylab', pch = 19,
                       quartileWt = 2,
                       quartileColor = 'gray81',
                       peakColor = 'gray27', ...){

  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  nm <- par("mar")
  nmfrow <- par('mfrow')
  noma <- par('oma')

  if (is.list(popn) & !is.data.frame(popn)) {
    if ("auto" %in% dimension) {
      temp <- attr(popn[[1]], "t")
      spat <- attr(popn[[1]], "s")
      comp <- attr(popn[[1]], "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if ("t" %in% dimension) {
        temp <- T
      }
      if ("s" %in% dimension) {
        spat <- T
      }
      if ("mt" %in% dimension) {
        comp <- T
      }
    }
    nr <- length(popn)
    nc <- sum(temp,spat,comp)
    par(mfrow = c(nr,nc))
    par(oma = c(4,3,2,1))

    count <- nrow(popn[[1]])
    emin <- min(popn[[1]]['x'])
    emax <- max(popn[[1]]['x'])
    nmin <- min(popn[[1]]['y'])
    nmax <- max(popn[[1]]['y'])
    smin <- min(as.numeric(popn[[1]][['s1']]))
    smax <- min(as.numeric(popn[[1]][['s1']]))
    opening <- min(popn[[1]]['start'])
    closing <- max(popn[[1]]['end'])

    for (i in 1:length(popn)){
      if (nrow(popn[[i]]) > max(count)){
        count <- nrow(popn[[i]])
      }
      if (min(popn[[i]]['x'])< emin){
        emin <- min(popn[[i]]['x'])
      }
      if (max(popn[[i]]['x'])< emax){
        emax <- max(popn[[i]]['x'])
      }
      if (min(popn[[i]]['y'])< nmin){
        nmin <- min(popn[[i]]['y'])
      }
      if (max(popn[[i]]['y'])< nmax){
        nmax <- max(popn[[i]]['y'])
      }
      if (max(as.numeric(popn[[i]][['s1']])>smax)){
        smax <- max(as.numeric(popn[[i]][['s1']]))
      }
      if (max(as.numeric(popn[[i]][['s2']])>smax)){
        smax <- max(as.numeric(popn[[i]][['s2']]))
      }
      if (min(as.numeric(popn[[i]][['s1']])>smin)){
        smin <- min(as.numeric(popn[[i]][['s1']]))
      }
      if (min(as.numeric(popn[[i]][['s2']])>smin)){
        smin <- min(as.numeric(popn[[i]][['s2']]))
      }
      if (min(popn[[i]]['start']) < opening){
        opening <- min(popn[[i]]['start'])
      }
      if (max(popn[[i]]['end']) < closing){
        closing <- min(popn[[i]]['end'])
      }
    }

    for (i in 1:length(popn)){
      popi <- popn[[i]]
      if (temp){     # temporal (flowering schedule)
        popi <- popi[order(popi[, 'start'], popi[, 'end']),]
        popi$index <- seq_along(popi[, 1])
        if (i == nr){
          par(mar = c(0.25,3.25,0.25,0.5))
          plot.default(popi[, 'start'], popi$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", xlab = 'date', ylab = "",
                        xaxt = 'n', ...)
          datLabs <- seq(opening, closing, by = 7)
          axis(1, at = datLabs, labels = format(as.Date(datLabs, origin = as.Date("1970-01-01")),format = "%b %d"), tick=0.25)
          segments(popi[, 'start'], popi$index, popi[, 'end'],
                   popi$index, col = "gray50", cex = 3, ...)
          mtext(names(popn)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
          mtext('date',side = 1,adj = 0.5, cex = 0.75, line = 3)
          mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        } else if(i == 1){
          par(mar = c(0.25,3.25,0.25,0.5))
          plot.default(popi[, 'start'], popi$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", ylab = "",
                       xaxt = "n", ...)
          segments(popi[, 'start'], popi$index, popi[, 'end'],
                   popi$index, col = "gray50", cex = 3, ...)
          mtext(names(popn)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
          mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
          mtext('temporal',side = 3, adj = 0.5, line = 0.5)
        }else{
          par(mar = c(0.25,3.25,0.25,0.5))
          plot.default(popi[, 'start'], popi$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", ylab = "",
                       xaxt = "n", ...)
          segments(popi[, 'start'], popi$index, popi[, 'end'],
                   popi$index, col = "gray50", cex = 3, ...)
          mtext(names(popn)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
          mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        }
        if (!is.null(sub)){
          segments(popi[popi$id %in% sub, 'start'], popi[popi$id %in% sub, 'index'], popi[popi$id %in% sub, 'end'],
                   popi[popi$id %in% sub, 'index'], col = "blue", ...)
        }
        if (dailyPoints == TRUE){
          rbd <- receptivityByDay(popi)
          fl.density <- colSums(rbd)
          points(as.numeric(names(fl.density)), fl.density, pch = pch, ...)
        }
        if (drawQuartiles ==TRUE){
          rbd <- receptivityByDay(popi)
          fl.density <- colSums(rbd)
          abline(v = median(popi$start), col = quartileColor, cex = quartileWt, lty = 2)
          abline(v = median(popi$end), col = quartileColor, cex = quartileWt, lty = 2)
          if (max(fl.density))
            abline(v = as.numeric(names(fl.density[fl.density == max(fl.density)])), col = peakColor, cex = quartileWt*1.5, ...)
        }
      }
      if (spat){     # spatial (map)
        xlab <- 'easting'
        ylab <- 'northing'
        if (i == nr){
          par(mar = c(0.25,3.25,0.25,0.5))
          plot.default(popi[, 'x'], popi[, 'y'], type = "n", xlab = xlab,
                      xlim = c(emin,emax), ylim = c(nmin,nmax), ylab = "", ...)
          mtext('easting',side = 1,adj = 0.5, cex = 0.75, line = 3)
          mtext('northing',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        } else if(i == 1){
          par(mar = c(0.25,3.25,0.25,0.5))
          plot.default(popi[, 'x'], popi[, 'y'], type = "n", xaxt = 'n',ylab = "",
                       xlim = c(emin,emax), ylim = c(nmin,nmax), ...)
          mtext('northing',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
          mtext('spatial',side = 3, adj = 0.5, line = 0.5)
        } else{
          par(mar = c(0.25,3.25,0.25,0.5))
          plot.default(popi[, 'x'], popi[, 'y'], type = "n", xaxt = 'n',ylab = "",
                        xlim = c(emin,emax), ylim = c(nmin,nmax), ...)
          mtext('northing',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        }

        if (is.null(pch)) {
          text(popi[, 'x'], popi[, 'y'], popi[, 'id'], ...)
        } else {
          points(popi[, 'x'], popi[, 'y'], pch = pch, ...)
        }
        if (!is.null(sub)){
          popi.sub <- popi[popi[, 'id'] %in% sub, ]
          text(popi.sub[, 'x'], popi.sub[, 'y'], popi.sub[, 'id'], pos = 3, ...)
          points(popi.sub[, 'x'], popi.sub[, 'y'], pch = 19, ...)
          popi.sub[, 'id']
          # mtext(names(popn[i]),2, adj = 0, outer = TRUE)
        }
        if(temp == F){
          mtext(names(popn)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        }
      }
      if(comp){
        popi$s1 <- as.numeric(popi$s1)
        popi$s2 <- as.numeric(popi$s2)
        for (j in 1:nrow(popi)){
          if (popi[j,'s1'] < popi[j,'s2']){
            popi[j,c('s1','s2')] <- popi[j,c('s2','s1')]
          }
        }
        ptWt<- aggregate(id ~ s1 + s2, data = popi, length)
        ptWt$scale <- (ptWt$id - min(ptWt$id)) / diff(range(ptWt$id))
        if (i == nr){
          par(mar = c(0.25,3.25,0.25,0.5))
          plot(ptWt$s1, ptWt$s2, cex = 2*ptWt$scale, pch = pch, xlim = c(smin, smax), ylim = c(smin,smax), ylab = "")
          mtext('s1',side = 1,adj = 0.5, cex = 0.75, line = 3)
          mtext('s2',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
          axis(2, at = smin:smax, labels = smin:smax, tick = 0.25)
          axis(1, at = smin:smax, labels = smin:smax)
        } else if(i == 1){
          par(mar = c(0.25,3.25,0.25,0.5))
          plot(ptWt$s1, ptWt$s2, cex = 2*ptWt$scale, pch = pch, xlim = c(smin, smax), ylim = c(smin,smax), xaxt = 'n', ylab = "")
          axis(2, at = smin:smax, labels = smin:smax, tick = 0.25)
          mtext('mating type',side = 3, adj = 0.5, line = 0.5)
          mtext('s2',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        }else {
          par(mar = c(0.25,3.25,0.25,0.5))
          plot(ptWt$s1, ptWt$s2, cex = 2*ptWt$scale, pch = pch, xlim = c(smin, smax), ylim = c(smin,smax), xaxt = "n", ylab = "")
          axis(2, at = smin:smax, labels = smin:smax, tick = 0.25)
          mtext('s2',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        }
        if (temp == F & spat == F){
          mtext(names(popn)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        }
      }
    }
  } else {

    if ("auto" %in% dimension) {
      temp <- attr(popn, "t")
      spat <- attr(popn, "s")
      comp <- attr(popn, "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if ("t" %in% dimension) {
        temp <- T
      }
      if ("s" %in% dimension) {
        spat <- T
      }
      if ("mt" %in% dimension) {
        comp <- T
      }
    }
    par(mfrow = c(1,sum(temp,spat,comp)))
    if (temp){     # temporal (flowering schedule)
      if (is.null(opening))
        opening <- min(popn[, 'start'])
      if (is.null(closing))
        closing <- max(popn[, 'end'])
      count <- dim(popn)[1]
      popn <- popn[order(popn[, 'start'], popn[, 'end']),]
      popn$index <- seq_along(popn[, 1])
      plot.default(popn[, 'start'], popn$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", xlab = 'date', ylab = 'count', xaxt = "n", ...)
      datLabs <- seq(opening, closing, by = 7)
      axis(1, at = datLabs, labels = format(as.Date(datLabs, origin = as.Date("1970-01-01")),
                                            format = "%b %d"),tick = 0.25)
      segments(popn[, 'start'], popn$index, popn[, 'end'],
               popn$index, col = "gray50", cex = 3, ...)


      if (!is.null(sub)){
        segments(popn[popn$id %in% sub, 'start'], popn[popn$id %in% sub, 'index'], popn[popn$id %in% sub, 'end'],
                 popn[popn$id %in% sub, 'index'], col = "blue", ...)
      }
      if (dailyPoints == TRUE){
        rbd <- receptivityByDay(popn)
        fl.density <- colSums(rbd)
        points(as.numeric(names(fl.density)), fl.density, pch = pch, ...)
      }
      if (drawQuartiles ==TRUE){
        rbd <- receptivityByDay(popn)
        fl.density <- colSums(rbd)
        abline(v = median(popn$start), col = quartileColor, cex = quartileWt, lty = 2)
        abline(v = median(popn$end), col = quartileColor, cex = quartileWt, lty = 2)
        if (max(fl.density))
          abline(v = as.numeric(names(fl.density[fl.density == max(fl.density)])), col = peakColor, cex = quartileWt*1.5, ...)
      }

    }

    if (spat){     # spatial (map)
      xlab <- 'easting'
      ylab <- 'northing'
      plot.default(popn[, 'x'], popn[, 'y'], type = "n", xlab = xlab, ylab = ylab, ...)
      if (is.null(pch)) {
        text(popn[, 'x'], popn[, 'y'], popn[, 'id'], ...)
      } else {
        points(popn[, 'x'], popn[, 'y'], pch = pch, ...)
      }
      if (!is.null(sub)){
        popn.sub <- popn[popn[, 'id'] %in% sub, ]
        text(popn.sub[, 'x'], popn.sub[, 'y'], popn.sub[, 'id'], pos = 3, ...)
        points(popn.sub[, 'x'], popn.sub[, 'y'], pch = 19, ...)
        popn.sub[, 'id']
      }
    }

    if(comp){
      popn$s1 <- as.numeric(popn$s1)
      popn$s2 <- as.numeric(popn$s2)
      for (i in 1:nrow(popn)){
        if (popn[i,'s1'] < popn[i,'s2']){
          popn[i,c('s1','s2')] <- popn[i,c('s2','s1')]
        }
      }
      ptWt<- aggregate(id ~ s1 + s2, data = popn, length)
      ptWt$scale <- (ptWt$id - min(ptWt$id)) / diff(range(ptWt$id))
      plot(ptWt$s1, ptWt$s2, cex = 2*ptWt$scale, xlab = "s1", ylab = "s2", pch = pch, xlim = c(min(ptWt$s2),max(ptWt$s1)), ylim = c(min(ptWt$s2),max(ptWt$s1)))
      axis(1, at = min(ptWt$s2):max(ptWt$s1), labels = min(ptWt$s2):max(ptWt$s1))
      axis(2, at = min(ptWt$s2):max(ptWt$s1), labels = min(ptWt$s2):max(ptWt$s1))
    }
  }
  par(mar = nm, mfrow = nmfrow, oma = noma)
}
