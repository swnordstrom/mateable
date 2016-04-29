##' Visualize a mating scene
##'
##' @title graphical visualization of a mating scene object
##' @param scene a matingScene object
##' @param dimension what dimension(s) of the mating scene should be visualized. Possible dimensions are 't' for temporal, 's' for spatial, 'mt' for mating type, and 'auto' (the default). For dimension = 'auto', all dimensions represented in the mating scene object will be plotted.
##' @param opening the number of days to adjust the start date displayed for the temporal dimension. Start date defaults to minimum day of year of start date in mating scene object.
##' @param closing the number of days to adjust the end date displayed for the temporal dimension. End date defaults to maximum day of year end date in mating scene object.
##' @param dailyPoints logical indicating whether daily counts of individuals should be displayed for plots of the temporal dimension
##' @param drawQuartiles logical indicating whether vertical lines should be drawn at population peak (see details) or quartiles
##' @param sortBy character indicating which columns to sort segments of flowering schedule by. Defaults to 'start', then 'end'. Up to three variables may be specified.
##' @param colorBy character optional, the name of a variable to use to assign color to segments or points.
##' @param sub a vector containing the ids of individuals to be highlighted in the plots or a character string specifying how to choose individuals to highlight. Possible values are "random" or "all". If NULL, no subset will be identified in the plots.
##' @param N a positive number, the number of individuals to sample if \code{sub} = 'random'
##' @param label.sub logical, indicating whether specified subset should be labeled
##' @param xlab.spat character label for x-axis of spatial dimension plots. If NULL, defaults to 'easting'.
##' @param ylab.spat character label for y-axis of spatial dimension plots. If NULL, defaults to 'northing'.
##' @param pch specify point type to be used in plots. Defaults to pch = 19 (filled-in circle). If NULL, points will be labeled with their id.
##' @param pt.cex specify point expansion factor (point size relative to device default)
##' @param text.cex specify text expansion factor (text size relative to device default)
##' @param plot.lim.spat if TRUE, spatial plot limits for lists of scenes are set by the maximum from all scenes
##' @param plot.lim.temp f TRUE, temporal plot limits for lists of scenes are set by the maximum from all scenes
##' @param quartile.lwd if \code{drawQuartiles} = TRUE, specifies weight of quartile and peak lines relative to device default.
##' @param quartile.col if \code{drawQuartiles} = TRUE, specifies color of quartile lines, defaults to 'gray81'.
##' @param peak.col if \code{drawQuartiles} = TRUE, specify color of peak lines, defaults to 'gray27'.
##' @param labelID if TRUE, the y-axis will be labeled with the id of the corresponding segment.
##' @param mt1 label for mating type '1', if dioecious
##' @param mt2 label for mating type '2', if dioecious
##' @param leg.ncol number of columns to include in legend, if colorBy is not NULL
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
                      sortBy = c('start','end'), colorBy = NULL,
                      sub= NULL, N = 3, label.sub = TRUE,
                      xlab.spat = NULL, ylab.spat = NULL,
                      pch = 19, pt.cex = 0.75, text.cex = 0.8,
                      plot.lim.spat  = TRUE, plot.lim.temp = TRUE,
                      quartile.lwd = 1, quartile.col = 'gray55', peak.col = 'gray27',
                      labelID = FALSE, mt1 = 'F', mt2 = 'M', leg.ncol = 1, ...){

  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  par.orig <- par("mar", "oma", "mfrow", "xpd")
  on.exit(par(par.orig))

  if (!is.list(scene[[1]])){
    scene <- list(scene)
  }

  if ("auto" %in% dimension) {
    temp <- attr(scene[[1]], "t")
    spat <- attr(scene[[1]], "s")
    comp <- attr(scene[[1]], "mt")
  } else {
    temp <- F
    spat <- F
    comp <- F
    if ("t" %in% dimension) temp <- T
    if ("s" %in% dimension) spat <- T
    if ("mt" %in% dimension) comp <- T
  }
  nr <- length(scene)
  nc <- sum(temp,spat,comp)
  par(mfrow = c(nr,nc), oma = c(5,3,4,1), xpd = F)
  if(is.null(colorBy)){
    par(oma = c(5,3,4,1))
  } else {
    par(oma = c(8,3,4,1))
  }

  if(spat){
    emin <- min(unlist(lapply(scene, function(x) x['x'])))
    emax <- max(unlist(lapply(scene, function(x) x['x'])))
    nmin <- min(unlist(lapply(scene, function(x) x['y'])))
    nmax <- max(unlist(lapply(scene, function(x) x['y'])))
  }

  if(temp){
    count <- max(unlist(lapply(scene, nrow)))
    if(is.null(opening)){
      opening <- min(unlist(lapply(scene, function(x) x['start'])))
    }
    if(is.null(closing)){
      closing <- max(unlist(lapply(scene, function(x) x['end'])))
    }
  }

  if(comp){
    smin <- min(unlist(lapply(scene, function(x) as.numeric(unlist(x[,c('s1','s2')])))))
    smax <- max(unlist(lapply(scene, function(x) as.numeric(unlist(x[,c('s1','s2')])))))
    if (length(unique(unlist(lapply(scene, function(x) as.numeric(unlist(x[,c('s1','s2')]))))))==2){
      dioecious <- T
      mtmax <- max(unlist(lapply(scene, function(x)table(x$s1))))
    } else {
      dioecious <- F
    }
  }

  if ('random' %in% sub){
    sub <- sample(unlist(lapply(scene, function(x)x['id'])), N)
  } else if ('all' %in% sub){
    sub <- unlist(lapply(scene, function(x)x['id']))
  }

  if(!is.null(colorBy)){
    colFactors <- as.factor(unlist(lapply(scene, function(x)x[,colorBy])))
    colLevels <- levels(colFactors)
    colDF <- data.frame(var = colLevels, color = I(rainbow(length(colLevels))))
  }

  for (i in 1:length(scene)){
    scene.i <- scene[[i]]
    orderVars <- scene.i[,sortBy]
    if (length(sortBy) == 1){
      scene.i <- scene.i[order(orderVars),]
    } else if (length(sortBy) == 2){
      scene.i <- scene.i[order(orderVars[,1],orderVars[,2]),]
    } else if (length(sortBy) == 3){
      scene.i <- scene.i[order(orderVars[,1],orderVars[,2], orderVars[,3]),]
    }
    scene.i$index <- seq_along(scene.i[, 1])
    par(mar = c(0.25,3.25,1,1))
    if (!is.null(colorBy)){
      if (is.numeric(scene.i[,colorBy])){
        palette(colorRampPalette(c('blue','red'))(9))
        vec <- seq(min(scene.i[,colorBy]), max(scene.i[,colorBy]), length.out = 9)
        scene.i$cols <- findInterval(scene.i[,colorBy],vec)
        cols.seg <- scene.i$cols
        cols.pt <- scene.i$cols
        cols.sub <- scene.i[scene.i$id %in% sub, 'cols']
      } else{

        col.i <- merge(scene.i,colDF, by.x = colorBy, by.y = 'var', all.x = T, sort = F)
        col.i <- col.i[order(col.i$index),]
        cols.seg <- col.i$color
        cols.pt <- col.i$color
        cols.sub <- col.i[col.i$id %in% sub, 'color']
      }
    } else {
      cols.seg <- 'gray50'
      cols.pt <- 'black'
      cols.sub <- 'blue'
    }

    if (temp){
      if (labelID){
        par(mar = c(0.25,7.25,0.25,1))
        if(!plot.lim.temp){
          plot.default(scene.i[, 'start'], scene.i$index, type = "n", xlab = 'date', ylab = "",xaxt = 'n',yaxt = 'n', ...)
        } else {
          plot.default(scene.i[, 'start'], scene.i$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", xlab = 'date', ylab = "",xaxt = 'n',yaxt = 'n', ...)
        }
        segments(scene.i[, 'start'], scene.i$index, scene.i[, 'end'],scene.i$index, col = cols.seg, cex = 3, ...)
        axis(2, labels = scene.i$id, at = scene.i$index, las = 1, cex.axis = 0.75)
        mtext(attr(scene.i,'originalNames')[1],side = 2,adj = 0.5, cex = 0.75, line = 7.5)
      } else {
        if(!plot.lim.temp){
          plot.default(scene.i[, 'start'], scene.i$index, type = "n", xlab = 'date', ylab = "",xaxt = 'n',yaxt = 'n', ...)
        }
        else {
          plot.default(scene.i[, 'start'], scene.i$index, ylim = c(1,count), xlim = c(opening, closing), type = "n", xlab = 'date', ylab = "",xaxt = 'n',yaxt = 'n', ...)
        }
        segments(scene.i[, 'start'], scene.i$index, scene.i[, 'end'],scene.i$index, col = cols.seg, cex = 3, ...)
        mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        axis(2)
      }
      mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2, las = 3)

      if (i == nr){
        datLabs <- seq(opening,closing, by = 7)
        axis(1, at = datLabs, labels = format(as.Date(attr(scene.i, 'origin') + datLabs, origin = as.Date("1970-01-01")),format = "%b %d"), tick=0.25, cex.axis = 0.9)
        mtext('date',side = 1,adj = 0.5, cex = 0.75, line = 3)
      }
      if (i == 1 & nc > 1){
        mtext('temporal',side = 3, adj = 0.5, line = 1.5)
      }
      if (!is.null(sub)){
        segments(scene.i[scene.i$id %in% sub, 'start'], scene.i[scene.i$id %in% sub, 'index'], scene.i[scene.i$id %in% sub, 'end'],scene.i[scene.i$id %in% sub, 'index'], col = cols.sub, ...)
        if(label.sub){
          text(scene.i[scene.i$id %in% sub, 'start']-0.02*closing, scene.i[scene.i$id %in% sub, 'index'], scene.i[scene.i$id %in% sub, 'id'], cex = text.cex)
        }
      }
      if (dailyPoints == TRUE){
        rbd <- receptivityByDay(scene.i)
        fl.density <- colSums(rbd)
        points(as.numeric(names(fl.density)), fl.density, pch = pch, cex = pt.cex, ...)
      }
      if (drawQuartiles ==TRUE){
        rbd <- receptivityByDay(scene.i)
        fl.density <- colSums(rbd)
        abline(v = median(scene.i$start), col = quartile.col, lwd = quartile.lwd, lty = 2)
        abline(v = median(scene.i$end), col = quartile.col, lwd = quartile.lwd, lty = 2)
        if (length(fl.density[fl.density == max(fl.density)])>1){
          peak <- median(as.numeric(names(fl.density[fl.density == max(fl.density)])))
          abline(v = peak, col = peak.col, cex = quartile.lwd, ...)
        } else {
          abline(v = as.numeric(names(fl.density[fl.density == max(fl.density)])), col = peak.col, cex = quartile.lwd, ...)
        }
      }
    }
    if (spat){
      if (is.null(xlab.spat)) xlab.spat <- 'easting'
      if (is.null(ylab.spat)) ylab.spat <- 'northing'
      if(!plot.lim.spat){
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = "",xaxt = 'n', asp = 1, cex = pt.cex, col = cols.pt, ...)
      } else {
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n",xlim = c(emin,emax), ylim = c(nmin,nmax), ylab = "",xaxt = 'n', asp = 1, cex = pt.cex, col = cols.pt, ...)
      }
      mtext(ylab.spat,side = 2,adj = 0.5, cex = 0.75, line = 2.5)
      if (i == nr){
        axis(1)
        mtext(xlab.spat,side = 1,adj = 0.5, cex = 0.75, line = 3)
      }
      if(i == 1 & nc > 1){
        mtext('spatial',side = 3, adj = 0.5, line = 1.5)
      }
      if (is.null(pch)) {
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'],cex = text.cex, col = cols.pt, ...)
      } else {
        points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, cex = pt.cex, col = cols.pt, ...)
      }
      if (!is.null(sub)){
        scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
        if(label.sub){
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3,xpd = T,cex = text.cex, ...)
        }
        points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = 19,col = cols.sub, cex = pt.cex, ...)
      }
      if(temp == F){
        mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2, las = 3)
      }
    }
    if(comp){
      if (dioecious){
        if (mt1 == 'F'){
          sr <- round(table(scene.i$s1)[2]/table(scene.i$s1)[1],digits = 2)
        } else {
          sr <- round(table(scene.i$s1)[1]/table(scene.i$s1)[2], digits = 2)
        }
        if (i == nr){
          barplot(table(scene.i$s1), col = 'gray27', ylab = '', names.arg = c(mt1,mt2), ylim = c(0,mtmax))
          mtext('mating type',side = 1,adj = 0.5, cex = 0.75, line = 3)
        }else {
          barplot(table(scene.i$s1), xaxt = 'n', col = 'gray27', ylab = '',ylim = c(0,mtmax))
        }
        leg.text <- paste('M/F sex ratio:',sr)
        mtext(leg.text, side = 3, adj = 0.5, bg = 'white', cex = 0.7, line=0.1)
        mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
      } else {
        scene.i$s1 <- as.numeric(scene.i$s1)
        scene.i$s2 <- as.numeric(scene.i$s2)
        for (j in 1:nrow(scene.i)){
          if (scene.i[j,'s1'] < scene.i[j,'s2']){
            scene.i[j,c('s1','s2')] <- scene.i[j,c('s2','s1')]
          }
        }
        ptWt<- aggregate(id ~ s1 + s2, data = scene.i, length)
        ptWt$scale <- (ptWt$id - min(ptWt$id)) / diff(range(ptWt$id))
        plot(ptWt$s1, ptWt$s2, cex = 2*ptWt$scale, pch = pch, xlim = c(smin, smax), ylim = c(smin,smax), ylab = "", xaxt = 'n', yaxt = 'n')
        mtext('s2',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        axis(2, at = smin:smax, labels = smin:smax, tick = 0.25)
        leg.text <- levels(as.factor(ptWt$id))
        legend('topleft',legend = leg.text, pt.cex = 1+(as.numeric(leg.text) - min(as.numeric(leg.text)))/diff(range(as.numeric(leg.text))), pch = pch)
        if (i == nr){
          mtext('s1',side = 1,adj = 0.5, cex = 0.75, line = 3)
          axis(1, at = smin:smax, labels = smin:smax)
        }
      }
      if(i == 1 & nc > 1){
        mtext('mating type',side = 3, adj = 0.5, line = 1.5)
      }
      if (temp == F & spat == F){
        mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2, las = 3)
      }
    }
  }
  if(!is.null(colorBy)){
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    if(is.numeric(scene.i[,colorBy])){
      legend('topleft', legend = c(round(vec,0)[9],'','','','','','','',round(vec,0)[1]), fill = colorRampPalette(c('red','blue'))(9), y.intersp = 0.68, title = colorBy, title.adj = 0.1, bty = 'n', adj = 0, x.intersp = 0.65, cex = 0.85)
    } else {
      legend('topleft', legend = as.character(colDF$var), fill = colDF$color, cex = 0.85, bty = 'n', title = colorBy, ncol = leg.ncol)
    }
  }
}

