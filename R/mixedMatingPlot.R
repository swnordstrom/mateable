##' Visualize multiple dimensions of a mating scene
##'
##' @title multi-dimensional visualization of mating scene object
##' @param scene a matingScene object
##' @param dimension what dimension(s) of the mating scene should be visualized. Possible dimensions are 't' for temporal, 's' for spatial, 'mt' for mating type, and 'auto' (the default). For dimension = 'auto', all dimensions represented in the mating scene object will be plotted.
##' @param sub a subset of the population to plot; either a character indicating whether to subset a random sample (\code{sub}='random'), all individuals (\code{sub}='all'), or a vector containing the IDs of the individuals to subset.
##' @param N if \code{sub} = 'random', the number of individuals to sample
##' @param xcoord x-axis coordinate system label
##' @param ycoord y-axis coordinate system label
##' @param pch point type, defaults to pch = 19, solid filled in circle. If pch = NULL, individuals will be labeled by their id.
##' @param ... optional arguments for the plot function
##' @return nothing
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' plot3DScene(pop)
##'
##'
##'
plot3DScene <- function(scene, dimension = "auto",
                        sub= NULL, ycoord = 'northing', xcoord = 'easting',
                        pch = 19, ...){
  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  nm <- par("mar")
  nmfrow <- par('mfrow')
  noma <- par('oma')

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

  if (!is.null(sub)){
    if(sub == 'random'){
      sub <- sample(unique(unlist(sapply(scene,function(x)x[,'id'], simplify = TRUE), use.names = F)),N)
    } else if(sub == 'all'){
      sub <-unique(unlist(sapply(scene,function(x)x[,'id'], simplify = TRUE), use.names = F))
    }
  }

  nr <- length(scene)
  par(mfrow = c(nr,1), oma = c(4,4,4,1),mar = c(1,6,0,1), xpd = T)


  if (spat){
    emin <- min(scene[[1]]['x'])
    emax <- max(scene[[1]]['x'])
    nmin <- min(scene[[1]]['y'])
    nmax <- max(scene[[1]]['y'])
  }

  if (comp){
    smin <- min(as.numeric(scene[[1]][['s1']]))
    smax <- min(as.numeric(scene[[1]][['s1']]))
  }

  if(temp){
    opening <- attr(scene[[1]],'origin')+min(scene[[1]][,'start'])
    closing <- attr(scene[[1]],'origin')+max(scene[[1]][,'end'])
    maxstart <- max(scene[[1]]['start'])
    minstart <- min(scene[[1]]['start'])
  }

  for (i in 1:length(scene)){
    if (spat){
      if (min(scene[[i]]['x'])< emin){
        emin <- min(scene[[i]][,'x'])
      }
      if (max(scene[[i]]['x'])> emax){
        emax <- max(scene[[i]][,'x'])
      }
      if (min(scene[[i]]['y'])< nmin){
        nmin <- min(scene[[i]][,'y'])
      }
      if (max(scene[[i]]['y'])> nmax){
        nmax <- max(scene[[i]][,'y'])
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
      if (attr(scene[[1]],'origin')+min(scene[[i]]['start']) < opening){
        opening <- attr(scene[[1]],'origin')+min(scene[[i]][,'start'])
      }
      if (attr(scene[[1]],'origin')+max(scene[[i]][,'end']) > closing){
        closing <- attr(scene[[1]],'origin')+max(scene[[i]][,'end'])
      }
      if (max(scene[[i]]['start']) > maxstart){
        maxstart <-max(scene[[i]]['start'])
      }
      if (min(scene[[i]]['start']) < minstart){
        minstart <-min(scene[[i]]['start'])
      }
    }
  }

  vec <- seq(minstart, maxstart, length.out = 9)
  palette(colorRampPalette(c('blue','red'))(9))

  for (i in 1:length(scene)){
    scene.i <- scene[[i]]
    if (temp){
      scene.i$cols <- findInterval(scene.i$start,vec)
    }
    if (temp & spat & comp){
      plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax), ylab = "", asp = 1, ...)
      mtext(ycoord, side = 2, cex = 0.75, adj = 0.5, line = 3)
      mtext(names(scene)[i],side = 2, cex = 0.75, font = 2, las = 1, adj = 0, line = 8)
      if (is.null(pch)) {
        palette(colorRampPalette(c('blue','red'))(9))
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
      } else {
        text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
          points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
        }
      }
      if (i == 1){
        mtext('spatial, temporal, and mating type plot', side = 3, line = 2, font = 2)
      }
      if(i == nr){
        mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 2)
        axis(1)
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        legend('topleft', legend = c(format(opening, format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))),format = "%b %d"),'',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")),fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = T, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03), cex = 0.75)
      }
    } else if (temp & spat){
      plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = "", xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax),asp = 1, ...)
      mtext(ycoord, side = 2, cex = 0.75, adj = 0.5, line = 3)
      mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 8, font = 2, las = 1)
      if (is.null(pch)) {
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
      } else {
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
          points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
        }
      }
      if (i == 1){
        mtext('spatial and temporal plot', side = 3, font = 2, line = 2)
      }
      if(i == nr){
        mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 3)
        axis(1)
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        legend('topleft', legend = c(format(opening, format = "%b %d"),' ',' ',' ', format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))),format = "%b %d"),'',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")),fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = T, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03), cex = 0.75)
      }
    } else if(spat & comp){
      plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = "", xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax), asp = 1, ...)
      mtext(ycoord, side = 2, cex = 0.75, adj = 0.5, line = 3)
      mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 8, font = 2, las = 1)
      if (is.null(pch)) {
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], ...)
      } else {
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, col = 'blue', ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,  ...)
          points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, ...)
          text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, ...)
          text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
        }
      }
      if (i == 1){
        mtext('spatial and mating type plot', font = 2, line = 2, side = 3)
      }
      if(i == nr){
        mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 3)
        axis(1)
      }
    } else if (temp & comp){
      par(mar = c(1,3,0,1),xpd = F)
      scene.i$s1 <- as.numeric(scene.i$s1)
      scene.i$s2 <- as.numeric(scene.i$s2)
      for (j in 1:nrow(scene.i)){
        if (scene.i[j,'s1'] < scene.i[j,'s2']){
          scene.i[j,c('s1','s2')] <- scene.i[j,c('s2','s1')]
        }
      }
      plot(jitter(scene.i[,'s1']), jitter(scene.i[,'s2']), col = scene.i$cols,xlim = c(min(scene.i$s2),max(scene.i$s1)), ylim = c(min(scene.i$s2),max(scene.i$s1)), xaxt = 'n',yaxt = 'n', xlab = '', ylab = '', pch = pch)
      mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 3.5, font = 2)
      mtext('s2', side = 2, outer = T, line = 1, cex = 0.75)
      abline(v = c(1:10) - 0.5, lty = 'dotted', col = 'lightgray')
      abline(h = c(1:10) - 0.5 , lty = 'dotted', col = 'lightgray')
      axis(2, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1), cex.axis = 0.75)
      if (!is.null(sub)){
        text(scene.i.sub[, 's1'], scene.i.sub[, 's2'], scene.i.sub[, 'id'], pos = 3, cex =1, font = 2, ...)
        scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
      }
      if (i == 1){
        title(main = 'temporal and compatibility plot', outer = T)
        legend('topleft', legend = c(format(opening, format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))), format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")), fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'o',xpd = T, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03), cex = 0.75)
      }
      if(i == nr){
        axis(1, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1), cex.axis = 0.75)
        mtext('s1',side = 1,adj = 0.5, cex = 0.75, line = 3)
      }
    }
  }
  par(mar = nm, mfrow = nmfrow, oma = noma, xpd = F)
}
