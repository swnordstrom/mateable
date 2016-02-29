##' Visualize multiple dimensions of mating potential
##'
##' @title graphical visualization of multiple mating potential objects
##' @param potentials
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
##' @seealso
##' @examples
##' pop <- simulateScene()
##' sync <- synchrony(pop)
##' potentialPlot(sync)
##'
##'

mixedPotentialPlot <-   function(matPots, # matPots is vector of matingPotential objects (must be length 1 to 3, each with unique dimension)
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

        # use apply function to subset each potential object in matPots for sub.ids
        extract.sub <- function(x){
          iids <- x[['ind']][['id']]
          diag(x[['pair']]) <- 1
          sub.iids <- x[['ind']][which(iids %in% sub.ids), 'id']
          subMat<- x[['pair']][which(sub.iids %in% attr(x[['pair']],'idOrder')),which(sub.iids %in% attr(x[['pair']],'idOrder'))]
        }

        poti <- vapply(matPots,extract.sub)

        if (t & s &! mt){
          # network diagram with lines weighted by synchrony, colored by proximity? id label size weighted by sum of product of synchrony and proximity for all interactions?
          # option for vice versa?
        }

        if (t & mt &! s){
          # network diagram with lines weighted by synchrony, lines omitted if incompatible
          # histogram with cells shaded by synchrony, omitted if imcompatible
        }

        if (s & mt &! t){
          # network diagram with lines weighted by proximity, lines omitted if incompatible
          # histogram with cells shaded by proximity, omitted if imcompatible
        }

        if (t & s & mt){
          # network diagram with lines weighted by synchrony, colored by proximity, lines omitted if incompatible, id label size weighted by sum of product of synchrony and proximity for all interactions?
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

      }

      if (t & mt &! s){

      }

      if (s & mt &! t){

      }

      if (t & s & mt){

      }
    }

  }
}


