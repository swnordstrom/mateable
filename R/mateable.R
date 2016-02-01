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

#' x <- generateFS()
#' x
#' plotFS(x)
#' pointsFS(x)
#' vlinesFS(x)
#' bs.x <- bootstrapFS(x)
#' addCIs(bs.x)
#' summaryFS(x)
#'
#' pop <- simulateScene()
#' pop
#' par(mfcol = c(1, 2))
#' plotFS(pop)
#' plotMap(pop)
#' par(mfcol = c(1, 1))
#'
#' str(eelr2012)
#' ee <- make3d(eelr2012, "firstDay", "lastDay", "Ecoord", "Ncoord", idcode = "tagNo")
#' plotFS(ee)
#' pointsFS(ee, type = "b")
#' plotMap(ee, cex = 0.5)
#' plotMap(ee, pch = 1, cex = 4)
#'
NULL
