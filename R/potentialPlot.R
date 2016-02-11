#### function to make mating potential plots ####

potentialPlot <-   function(matPot,
                            plotType = c('hist','net','heat'),
                            showDensity = T,
                            potentialType = c('ind','pair'),
                            # pairwise.measure = c("synchrony", "proximity", "asynchrony", "distance"),
                            sub.ids = NULL, N = 9, sample = "random",
                            sub.labels = FALSE,
                            # sub.lines = FALSE,
                            # ind.nn = 5, ind.measure = NULL,
                            ind.labels = TRUE,
                            lab.cex = 0.5, main = NULL, ...){

  if(!sample %in% c("random", "all")) {warning("sample must be 'random' or 'all'")}

  if(attr(matPot,'t')){
    potential <- 'synchrony'
  } else if(attr(matPot,'s')){
    potential <- 'proximity'
  } else if(attr(matPot,'c')){
    potential <- 'compatibility'
  }

  pt <- match.arg(plotType)
  potentialType <- match.arg(potentialType)
  # pm <- match.arg(pairwise.measure)
  ids <- matPot$ind$id
  if(is.null(main) & potentialType %in% 'ind') main <- paste('average pairwise', potential)
  if(is.null(main) & potentialType %in% 'pair') main <- paste('pairwise', potential)
  if (is.null(sub.ids)){
    if (sample %in% 'all'){
      sub.ids <- ids
    } else {
      sub.ids <- sample(ids, N)
    }
  }

  if (potentialType %in% 'pair'){
    if (pt %in% 'hist'){
      hist(matPot[[potentialType]], prob = TRUE, breaks = 15, main = main, xlab = potential)
      if (showDensity){
        lines(density(matPot[[potentialType]]))
      }
    } else {
      subMat<- matPot$pair[which(sub.ids %in% attr(matPot$pair,'idOrder')),which(sub.ids %in% attr(matPot$pair,'idOrder'))]
#       print(sub.ids)
#       print(subMat)
      if (pt %in% 'heat'){
        subMat[upper.tri(subMat, diag = FALSE)] <- NA
        par(mgp = c(0.1,0.1,0.1))
        image(x = 1:nrow(subMat),y = 1:nrow(subMat), z = subMat, axes = F, xlab = "", ylab = "", col = heat.colors(12))
        title(main = paste('pairwise',potential, 'heatmap'))
        axis(1, 1:ncol(subMat), labels = sub.ids, tick = 0, cex.axis = -0.2 + 1/log10(nrow(subMat)))
        axis(4, 1:ncol(subMat), labels = sub.ids, tick = 0, cex.axis = -0.2 + 1/log10(nrow(subMat)), las = 2)
        par(mgp = c(3,1,0))
      } else if (pt %in% 'net'){
        subMat[lower.tri(subMat, diag = TRUE)] <- 0
        if(ind.labels){
          im <- matPot[['ind']][which(sub.ids %in% ids), potential]
          lab.cex <- 1 + (im - min(im))/(max(im) - min(im))
        }
        plotweb3(subMat, names = sub.ids, main = main, val = FALSE, legend = FALSE, length = 0,
                 labz.size = lab.cex, ...)
      }
    }
  } else if (potentialType %in% 'ind') {
    subVals <- matPot[['ind']][which(sub.ids %in% ids), potential]
    if (pt %in% 'hist'){
      hist(matPot[[potentialType]][,potential], prob = TRUE, breaks = 15, main = main,xlab = potential)
      if (showDensity){
        lines(density(matPot[[potentialType]][,potential]))
      }

    } else (warning("plotType for potentialType = 'ind' must be histogram"))
  }
}


# test <- simulateScene(size = 30, sAlleles = 10)
# syncPot <- synchrony(test, method = 'overlap',synchronyType = 'all',compareToSelf = F)
# proxPot <- proximity(test, method = 'maxProp')

## test it out:
potentialPlot(proxPot, potentialType = 'pair', plotType = 'heat')

### Add a way to view sub.ids in histogram by drawing line/label where the subset inds are in the distribution??

#     if (sub.lines){
#       arrows(x0 = subVals, x1 = subVals, y0 = 0, y1 = max(hist(matPot$pair, prob= TRUE)$density)* 0.9)
#     }
#     if(sub.labels){
#       text(x = subVals*1.1, y = max(hist(matPot$pair, prob= TRUE)$density)* 0.8, labels = sub, cex = .7)
#     }

  #### '2D' potential plots ###
  # s & t : time series of maps, options to choose temporal scale of resolution (how many days/years displayed)
  # s & c : two panel display with map and heatmap or a network diagram showing the compatibility of a subset
  #         connect points on map with lines if compatible
  # t & c : two panel display with flowering schedule/network diagram/heatmap
  #         time series of heatmaps/network diagrams, options to choose temporal scale of resolution (how many days/years displayed)
