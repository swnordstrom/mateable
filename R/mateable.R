#' A package to simulate, manage, visualize, and jointly analyze spatially and
#' temporally explicit datasets of mating potential
#'
#' This package faciltates the investigation of three dimensions of mating
#' potential. It provides a method for simulating populations and includes a
#' dataset.
#'
#' @docType package
#' @name mateable-package
#' @aliases mateable mateable-package
#' @author Stuart Wagenius, Danny Hanson, Amy Waananen
#' @references Background: \url{http://echinaceaProject.org/}
#' @examples
#' library(mateable)

#'
#' pop <- simulateScene()
#' pop
#'
#' matingPlot(pop)
#' matingPlot(pop, "t")
#'
#' mixedMatingPlot(pop)
#'
#' sync <- synchrony(pop, "augs")
#' prox <- proximity(pop, "maxPropSqrd")
#' potentialPlot(sync)
#' potentialPlot(prox, "ind")
#'
#'
#' str(eelr2012)
#' ee <- makeScene(eelr2012, FALSE, "firstDay", "lastDay", "Ecoord", "Ncoord", idCol = "tagNo")
#'
NULL
