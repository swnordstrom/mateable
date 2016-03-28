##' Visualize multiple dimensions of mating potential
##'
##' @title graphical visualization of multiple mating potential objects
##' @param matPot vector, contains one or multiple mating potential objects with unique potential types
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
##' plot3DPotential(c(sync,prox))
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

  if(!sample %in% c("random", "all")) {warning("sample must be 'random' or 'all'")}

  if (is.null(subject)){
    if (!'pair' %in% names(matPot[[1]])){
      subject <- 'ind'
    } else {
      subject <- 'pair'
    }
  }

  len <- ifelse(is.list(matPots[[1]]),length(matPots[[1]]), 1)

  if (is.null(sub.ids)){
    if(sample == 'random'){
      sub <- sample(unique(unlist(sapply(matPots[1], function(x)x$ind$id), use.names = F)),N)
    } else if(sample == 'all'){
      sub <-unique(unlist(sapply(matPots[1], function(x)x$ind$id), use.names = F))
    }
  }

  for (i in 1:length(matPots)){
    matPot <- matPots[i]
    if (!is.list(matPot[[1]])){matPot <- list(matPot)}

    if (length(matPot) != len){warning('matPot objects are not the same length')}

    if(attr(matPot[[1]],'t')){
      synchrony <- matPot
    } else if(attr(matPot[[1]],'s')){
      proximity <- matPot
    } else if(attr(matPot[[1]],'c')){
      compatibility <- matPot
    }
  }

  if(!'pair' %in% names(matPots[[1]]) & 'pair' %in% subject){
      warning("mating potential object must have pairwise potential for subject to be 'pair'")
      subject <- 'ind'
  }

  if(is.null(main) & subject %in% 'ind') main <- paste('individual', potential)
  if(is.null(main) & subject %in% 'pair') main <- paste('pairwise', potential)


  par(mfrow = c(len,1))

  if (subject %in% 'ind'){
    par(oma = c(1,1,2,0))
  } else {
    par(mar = c(4,0.5,0.5,2.5))
    par(oma = c(4,4,4,1.5))
  }

  if ('ind' %in% subject){
    hmax <- max(hist(matPot[[1]][[subject]][,potential], breaks = 15, plot = F)$breaks)
    hmin <- min(hist(matPot[[1]][[subject]][,potential], breaks = 15, plot = F)$breaks)
  } else {
    hmax <- max(hist(matPot[[1]][[subject]], breaks = 15, plot = F)$breaks)
    hmin <- min(hist(matPot[[1]][[subject]], breaks = 15, plot = F)$breaks)
  }

  for (j in 1:length(matPot)){
    potj <- matPot[[j]]
    if('hist' %in% pt){
      if('ind' %in% subject){
        if (max(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks) > hmax){
          hmax <- max(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks)
        }
        if (min(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks) < hmin){
          hmin <- min(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks)
        }
      } else {
        if (max(hist(potj[[subject]], breaks = 15, plot = F)$breaks) > hmax){
          hmax <- max(hist(potj[[subject]], breaks = 15, plot = F)$breaks)
        }
        if (min(hist(potj[[subject]], breaks = 15, plot = F)$breaks) < hmin){
          hmin <- min(hist(potj[[subject]], breaks = 15, plot = F)$breaks)
        }
      }
    }
  }



}
