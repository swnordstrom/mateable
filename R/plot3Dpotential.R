##' Visualize multiple dimensions of mating potential
##'
##' @title graphical visualization of multiple mating potential objects
##' @param matPots list, contains one or multiple mating potential objects representing unique potential dimensions
##' @param subject character, indicates whether the subject to be visualized is individuals (\code{subject} = 'ind') or all pairwise interactions (\code{subject} = 'pair')
##' @param density logical, if TRUE (default), plots probability density over histogram
##' @param sub.ids vector, contains the IDs of individuals to be represented in pairwise potential plots
##' @param N integer, indicates the number of individuals to sample if sub.ids = 'random', default N = 9
##' @param sample character, specifies how to sample individuals to be represented in pairwise potential plots. Possible values are "random" (default) or "all". See details.
##' @param main character, the main plot title
##' @param ... optional arguments for the plot function
##' @details The individuals to be represented in the pairwise potential plots can either be specified explicitly through \code{sub.ids}, chosen randomly (\code{sample} = 'random'), or all individuals can be selected (\code{sample} = 'all'). The default is to randonly select 9 individuals. If multiple years are being plotted, the subset is sampled from all years and the same individuals will be represented in each year, if possible. If fewer than three individuals from the subset are available in a year, no network diagram or heatmap will be returned for that year.
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' sync <- synchrony(pop, "augs")
##' prox <- proximity(pop, 'maxDist')
##' plot3DPotential(list(sync,prox))
##'
##'
##'
##'
plot3DPotential <-   function(matPots,
                              subject = NULL,
                              density = TRUE,
                              sub.ids = NULL, N = 9, sample = "random",
                              main = NULL, ...){
  nm <- par("mar")
  noma <- par('oma')
  nmfrow <- par('mfrow')

  if(!sample %in% c("random", "all")) {stop("sample must be 'random' or 'all'")}

  if(length(unique(sapply(v1, length))) != 1) {stop('mating potential objects are different lengths')}

  if (is.null(subject)){
    if (!'pair' %in% names(matPots[[1]])){
      subject <- 'ind'
    } else {
      subject <- 'pair'
    }
  }

  synchrony <- F
  proximity <- F
  compatibility <- F

  for (i in 1:length(matPots)){
    matPot <- matPots[i]

    if (is.null(sub.ids)){
      if(sample == 'random'){
        sub.ids <- sample(unique(unlist(sapply(matPot, function(x)x$ind$id), use.names = F)),N)
      } else if(sample == 'all'){
        sub.ids <-unique(unlist(sapply(matPot, function(x)x$ind$id), use.names = F))
      }
    }

    if(attr(matPot[[1]],'t')){
      synchrony <- T
      sync <- matPot
    } else if(attr(matPot[[1]],'s')){
      proximity <- T
      prox <- matPot
    } else if(attr(matPot[[1]],'c')){
      compatibility <- T
      compat <- matPot
    }
  }

  ndim <- sum(c(synchrony, proximity, compatibility))

  if(!'pair' %in% names(matPots[[1]]) & 'pair' %in% subject){
    warning("mating potential object does not include pairwise potential; displaying subject = 'ind'")
    subject <- 'ind'
  }

  if(is.null(main) & subject %in% 'ind') main <- paste('individual potential')
  if(is.null(main) & subject %in% 'pair') main <- paste('pairwise potential')

  len <- unique(sapply(matPots, length))
  par(mfrow = c(len,1))

  if (subject %in% 'ind'){
    par(oma = c(1,1,2,0))
  } else {
    par(mar = c(4,0.5,0.5,2.5))
    par(oma = c(4,4,4,1.5))
  }

  if (synchrony & proximity){
    if(subject %in% 'ind'){
      ind <- mapply(function(x1,x2) merge(x1$ind, x2$ind), sync, prox, SIMPLIFY = F)
    } else {
      pair <- mapply(function(x1,x2) array(c(x1$pair, x2$pair), dim = c(dim(x1$pair)[1],dim(x1$pair)[1],ndim)), sync, prox, SIMPLIFY = F)
    }
  } else if (synchrony & compatibility){
    if (subject %in% 'ind'){
      ind <- mapply(function(x1,x2) merge(x1[['ind']], x2[['ind']]), sync, compat, SIMPLIFY = F)
    } else {
      pair <- mapply(function(x1,x2) array(c(x1$pair, x2$pair), dim = c(dim(x1$pair)[1],dim(x1$pair)[1],ndim)), sync, compat, SIMPLIFY = F)
    }
  } else if (proximity & compatibility){
    if(subject %in% 'ind'){
      ind <- mapply(function(x1,x2) merge(x1[['ind']], x2[['ind']]), prox, compat, SIMPLIFY = F)
    } else {
      pair <- mapply(function(x1,x2) array(c(x1$pair, x2$pair), dim = c(dim(x1$pair)[1],dim(x1$pair)[1],ndim)), sync, prox, SIMPLIFY = F)
    }
  } else if (synchrony & proximity & compatibility){
    if(subject %in% 'ind'){
      ind <- mapply(function(x1,x2, x3) merge(merge(x1[['ind']], x2[['ind']]),x3[['ind']]), sync, prox, compat, SIMPLIFY = F)
    } else {
      pair <- mapply(function(x1,x2,x3) array(c(x1$pair, x2$pair, x3$pair), dim = c(dim(x1$pair)[1],dim(x1$pair)[1],ndim)), sync, prox,compat, SIMPLIFY = F)
    }
  }
}



# pop <- simulateScene()
# sync <- synchrony(pop, "augs")
# prox <- proximity(pop, 'maxProp')
plot3DPotential(list(sync,prox), subject = 'pair')

# p <- list(y1 = prox, y2 = prox, y3 = prox, y4 = prox)
# s <- list(y1 = sync, y2 = sync, y3 = sync, y4 = sync)
#
# v1 <- list(proximity = p , synchrony = s)
