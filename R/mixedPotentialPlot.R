
##' Visualize multiple dimensions of mating potential
##'
##' @title graphical visualization of multiple mating potential objects
##' @param matPots
##' @param plotType
##' @param showDensity
##' @param sub.ids
##' @param N
##' @param sample
##' @param sub.labels
##' @param ind.labels
##' @param lab.cex
##' @param main
##' @param ...
##' @return nothing
##' @return optional arguments for the plot function
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' sync <- synchrony(pop)
##' potentialPlot(sync)
##'
##'


potentialPlot <-   function(matPots, # matPots is vector of matingPotential objects (must be length 1 to 3, each with unique dimension)
                            showDensity = T,
                            sub.ids = NULL, N = 9, sample = "random",
                            sub.labels = FALSE,
                            ind.labels = TRUE,
                            lab.cex = 0.5, main = NULL, ...){

  nm <- par("mar")
  noma <- par('oma')
  nmfrow <- par('mfrow')

  if(!sample %in% c("random", "all")) {warning("sample must be 'random' or 'all'")}


  if (length(matPots) == 1) {
    potentialPlot(matPots)
  } else if (length(matPots) <= 3){
    if (is.list(matPots[1][[1]])) { # if multi-year potential objects

      nr <- length(matPots[1])
      par(mfrow = c(nr,nc))
      par(mar = c(4,0.5,0.5,2.5))
      par(oma = c(4,4,2.25,1.5))


      for (i in 1:length(matPots)){
        if(attr(matPots[[i]],'t')){
          t <- TRUE
        }
        if(attr(matPots[[i]],'s')){
          s <- TRUE
        }
        if(attr(matPots[[i]],'mt')){
          mt <- TRUE
        }

        ### might want to do some work to make sure that matPots object has dimensions in the same order every time (t, s, mt)

        # use apply function to subset each potential object in matPots for sub.ids
        extract.sub <- function(x){
          iids <- x[['ind']][['id']]
          diag(x[['pair']]) <- 1
          sub.iids <- x[['ind']][which(iids %in% sub.ids), 'id']
          subMat<- x[['pair']][which(sub.iids %in% attr(x[['pair']],'idOrder')),which(sub.iids %in% attr(x[['pair']],'idOrder'))]
        }

        poti <- vapply(matPots,extract.sub)

        if (t & s &! mt){

          # map with points colored by DAM
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
          }
          mtext(names(matPots)[[i]],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)

        }

        if (t & mt &! s){
          # mating type alleles colored by DAM
        }

        if (s & mt &! t){
          # map with points colored by mating type alleles (two color points??)
          # map with lines drawn between compatible plants
        }

        if (t & s & mt){
          # map with points colored by time and lines drawn between compatible individuals
        }

      }

    } else {
      for (i in 1:length(matPots)){
        if(attr(matPots[i],'t')){
          t <- TRUE
        } else if(attr(matPots[i],'s')){
          s <- TRUE
        } else if(attr(matPots[i],'mt')){
          mt <- TRUE
        }
      }

      # if matPot is not a list
      if (t & s &! mt){
        # map with points colored by DAM
      }

      if (t & mt &! s){
        # mating type alleles colored by DAM
      }

      if (s & mt &! t){
        # map with points colored by mating type alleles (two color points??)
        # map with lines drawn between compatible plants
      }

      if (t & s & mt){
        # map with points colored by time and lines drawn between compatible individuals
      }
    }

  }
}


