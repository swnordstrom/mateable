##' Visualize a mating scene
##'
##' @title graphical visualization of a mating scene object
##' @param scene a matingScene object
##' @param dimension what dimension(s) of the mating scene should be visualized. Possible dimensions are 't' for temporal, 's' for spatial, 'mt' for mating type, and 'auto' (the default). For dimension = 'auto', all dimensions represented in the mating scene object will be plotted.
##' @param opening the number of days to adjust the start date displayed for the temporal dimension. Start date defaults to minimum day of year of start date in mating scene object.
##' @param closing the number of days to adjust the end date displayed for the temporal dimension. End date defaults to maximum day of year end date in mating scene object.
##' @param dailyPoints logical indicating whether daily counts of individuals should be displayed for plots of the temporal dimension
##' @param drawQuartiles logical indicating whether vertical lines should be drawn at population peak (see details) or quartiles
##' @param sub a vector containing the ids of individuals to be highlighted in the plots or a character string specifying how to choose individuals to highlight. Possible values are "random" or "all". If NULL, no subset will be identified in the plots.
##' @param N a positive number, the number of individuals to sample if \code{sub} = 'random'
##' @param xcoord label for x coordinate of spatial dimension plots. If NULL, defaults to 'easting'.
##' @param ycoord label for y coordinate of spatial dimension plots. If NULL, defaults to 'northing'.
##' @param pch specify point type to be used in plots. Defaults to pch = 19 (filled in circle). If NULL points will be labeled by id.
##' @param quartileWt if drawQuartiles = TRUE, specify weight of quartile and peak lines
##' @param quartileColor if drawQuartiles = TRUE, specify color of quartile lines, defaults to 'gray81'
##' @param peakColor if drawQuartiles = TRUE, specify color of peak lines, defaults to 'gray27'
##' @param ... standard graphical parameters
##' @return nothing
##' @return optional arguments for the plot function
##' @details Population peak is defined by when maximum number individuals were reproductively receptive on one day. If multiple days had the same maximum number, peak is defined as the median of these dates.
##' @export
##' @author Amy Waananen
##' @seealso see \code{\link{plot3DScene}} to visualize multiple dimensions on one plot
##' @examples
##' pop <- simulateScene()
##' plotScene(pop)
##' \dontrun{plotMap(NULL)}
##'
##'
plotScene <- function(scene, dimension = "auto",
                      opening = NULL, closing = NULL,
                      dailyPoints = TRUE, drawQuartiles = TRUE,
                      sub= NULL, N = 9, xcoord = NULL, ycoord = NULL, pch = 19,
                      quartileWt = 1,
                      quartileColor = 'gray55',
                      peakColor = 'gray27', ...){

  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  nm <- par("mar")
  nmfrow <- par('mfrow')
  noma <- par('oma')
  par(xpd = F)

  if (!is.list(scene[[1]])){
    scene <- list(scene)
  }

  if (is.list(scene) & !is.data.frame(scene)) {
    if ("auto" %in% dimension) {
      temp <- attr(scene[[1]], "t")
      spat <- attr(scene[[1]], "s")
      comp <- attr(scene[[1]], "mt")
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
    nr <- length(scene)
    nc <- sum(temp,spat,comp)
    par(mfrow = c(nr,nc))
    par(oma = c(5,3,4,1))

    if(spat){
      emin <- min(scene[[1]]['x'])
      emax <- max(scene[[1]]['x'])
      nmin <- min(scene[[1]]['y'])
      nmax <- max(scene[[1]]['y'])
    }

    if(temp){
      if(is.null(opening)){
        opening <- min(scene[[1]]['start'])
      }
      if(is.null(closing)){
        closing <- max(scene[[1]]['end'])
      }
    }

    if(comp){
      smin <- min(as.numeric(scene[[1]][['s1']]))
      smax <- min(as.numeric(scene[[1]][['s1']]))
    }

    count <- nrow(scene[[1]])

    for (i in 1:length(scene)){
      if (nrow(scene[[i]]) > max(count)){
        count <- nrow(scene[[i]])
      }

      if (spat){
        if (min(scene[[i]]['x'])< emin){
          emin <- min(scene[[i]]['x'])
        }
        if (max(scene[[i]]['x'])> emax){
          emax <- max(scene[[i]]['x'])
        }
        if (min(scene[[i]]['y'])< nmin){
          nmin <- min(scene[[i]]['y'])
        }
        if (max(scene[[i]]['y'])> nmax){
          nmax <- max(scene[[i]]['y'])
        }
      }

      if(comp){
        if (max(as.numeric(scene[[i]][['s1']])>smax)){
          smax <- max(as.numeric(scene[[i]][['s1']]))
        }
        if (max(as.numeric(scene[[i]][['s2']])>smax)){
          smax <- max(as.numeric(scene[[i]][['s2']]))
        }
        if (min(as.numeric(scene[[i]][['s1']])<smin)){
          smin <- min(as.numeric(scene[[i]][['s1']]))
        }
        if (min(as.numeric(scene[[i]][['s2']])<smin)){
          smin <- min(as.numeric(scene[[i]][['s2']]))
        }
      }

      if(temp){
        if(is.null(opening)){
          if (min(scene[[i]]['start']) < opening){
            opening <- min(scene[[i]]['start'])
          }
        }
        if(is.null(closing)){
          if (max(scene[[i]]['end']) > closing){
            closing <- max(scene[[i]]['end'])
          }
        }
      }
    }

    if ('random' %in% sub){
      sub <- sample(scene[[1]][['id']],9)
    }

    for (i in 1:length(scene)){
      scene.i <- scene[[i]]
      par(mar = c(0.25,3.25,0.25,1))
      if (temp){     # temporal (flowering schedule)
        scene.i <- scene.i[order(scene.i[, 'start'], scene.i[, 'end']),]
        scene.i$index <- seq_along(scene.i[, 1])
        plot.default(scene.i[, 'start'], scene.i$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", xlab = 'date', ylab = "",xaxt = 'n', ...)
        segments(scene.i[, 'start'], scene.i$index, scene.i[, 'end'],scene.i$index, col = "gray50", cex = 3, ...)
        mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        if (i == nr){
          datLabs <- seq(opening,closing, by = 7)
          axis(1, at = datLabs, labels = format(as.Date(attr(scene.i, 'origin') + datLabs, origin = as.Date("1970-01-01")),format = "%b %d"), tick=0.25)
          mtext('date',side = 1,adj = 0.5, cex = 0.75, line = 3)
        }
        if (i == 1){
          mtext('temporal',side = 3, adj = 0.5, line = 1.5)
        }
        if (!is.null(sub)){
          segments(scene.i[scene.i$id %in% sub, 'start'], scene.i[scene.i$id %in% sub, 'index'], scene.i[scene.i$id %in% sub, 'end'],scene.i[scene.i$id %in% sub, 'index'], col = "blue", ...)
        }
        if (dailyPoints == TRUE){
          rbd <- receptivityByDay(scene.i)
          fl.density <- colSums(rbd)
          points(as.numeric(names(fl.density)), fl.density, pch = pch, ...)
        }
        if (drawQuartiles ==TRUE){
          rbd <- receptivityByDay(scene.i)
          fl.density <- colSums(rbd)
          abline(v = median(scene.i$start), col = quartileColor, lwd = quartileWt, lty = 2)
          abline(v = median(scene.i$end), col = quartileColor, lwd = quartileWt, lty = 2)
          if (length(fl.density[fl.density == max(fl.density)])>1){
            peak <- median(as.numeric(names(fl.density[fl.density == max(fl.density)])))
            abline(v = peak, col = peakColor, cex = quartileWt, ...)
          } else {
            abline(v = as.numeric(names(fl.density[fl.density == max(fl.density)])), col = peakColor, cex = quartileWt, ...)
          }
        }
      }
      if (spat){     # spatial (map)
        if (is.null(xcoord)){
          xcoord <- 'easting'
        }
        if (is.null(ycoord)){
          ycoord <- 'northing'
        }
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n",
                     xlim = c(emin,emax), ylim = c(nmin,nmax), ylab = "",xaxt = 'n', asp = 1,...)
        mtext(ycoord,side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        if (i == nr){
          axis(1)
          mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 3)
        }
        if(i == 1){
          mtext('spatial',side = 3, adj = 0.5, line = 1.5)
        }
        if (is.null(pch)) {
          text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], ...)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, ...)
        }
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3,xpd = T, ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = 19,col = 'blue', ...)
        }
        if(temp == F){
          mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        }
      }
      if(comp){
        scene.i$s1 <- as.numeric(scene.i$s1)
        scene.i$s2 <- as.numeric(scene.i$s2)
        for (j in 1:nrow(scene.i)){
          if (scene.i[j,'s1'] < scene.i[j,'s2']){
            scene.i[j,c('s1','s2')] <- scene.i[j,c('s2','s1')]
          }
        }
        ptWt<- aggregate(id ~ s1 + s2, data = scene.i, length)
        ptWt$scale <- (ptWt$id - min(ptWt$id)) / diff(range(ptWt$id))
        plot(ptWt$s1, ptWt$s2, cex = 2*ptWt$scale, pch = pch, xlim = c(smin, smax), ylim = c(smin,smax), ylab = "", xaxt = 'n')
        mtext('s2',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        axis(2, at = smin:smax, labels = smin:smax, tick = 0.25)
        leg.text <- levels(as.factor(ptWt$id))
        legend('topleft',legend = leg.text, pt.cex = 1+(as.numeric(leg.text) - min(as.numeric(leg.text)))/diff(range(as.numeric(leg.text))), pch = pch)
        if (i == nr){
          mtext('s1',side = 1,adj = 0.5, cex = 0.75, line = 3)
          axis(1, at = smin:smax, labels = smin:smax)
        }
        if(i == 1){
          mtext('mating type',side = 3, adj = 0.5, line = 1.5)
        }
        if (temp == F & spat == F){
          mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        }
      }
    }
  }
  par(mar = nm, mfrow = nmfrow, oma = noma)
}
