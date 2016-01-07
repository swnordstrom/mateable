##' Extract spatial columns from a 3d population object
##'
##' @title Get a data frame with only spatial data
##' @param popn a 3d population object
##' @return a data frame with only the id, x, and y columns from the
##' 3d population object
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' space <- getSpatialDF(pop)
getSpatialDF <- function(popn) {
  id <- popn$df[[popn$id]]
  x <- popn$df[[popn$x]]
  y <- popn$df[[popn$y]]
  data.frame(id = id, x = x, y = y)
}

##' Compute all pairwise distances for a population. This function
##' is simply a wrapper for \code{dist} that only returns the vector
##'
##' @title Distance matrix for a population
##' @param popn a 3D population object
##' @return a vector representing the lower triangle of the distance matrix
##' @export
##' @seealso \code{\link{dist}} \code{\link{getDistij}}
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' distance <- getPairwiseDistance(pop)
##' getDistij(distance, 14, 15)
getPairwiseDistance <- function(popn) {
  distances <- dist(popn$df[,c(popn$x, popn$y)])
  n <- attr(distances, "Size")
  dist.num <- as.numeric(distances)
  attr(dist.num, "n") <- n
  attr(dist.num, "includeSelf") <- F
  attr(dist.num, "indices") <- popn$df[[popn$id]]
  dist.num
}

##' Find the k nearest neighbors for all individuals in a population. This
##' function is simply a wrapper for \code{FNN::knn.dist}.
##'
##' @title Get k Nearest Neighbors
##' @param popn a 3D population object
##' @param k integer of how many nearest neighbors to get
##' @return a matrix where the rows are all individuals and the columns are
##' their k nearest neighbors
##' @export
##' @seealso \code{\link{knn.dist}}
##' @author Danny Hanson
##' @examples
##' pop <- generatePop(10)
##' kNearNeighbors(pop, 3)
kNearNeighbors <- function(popn, k) {
  knnMatrix <- FNN::knn.dist(popn$df[c(popn$x, popn$y)], k = k, algorithm = "brute")
  rownames(knnMatrix) <- popn$df[, popn$id]
  colnames(knnMatrix) <- paste("k", 1:k, sep = "")
  knnMatrix
}

##' Currently the function doesn't do anything. If you like returning null,
##' I would use it a lot.
##'
##' @title Spatial proximity of a population
##' @param popn a 3D population object
##' @param proximityFun a function used to calculate proximity
##' @param proximityType whether you want individual, population, or both
##' @return NULL
##' @export
##' @author No Body
##' @examples
##' pop <- generatePop()
##' proximity(pop)
proximity <- function(popn, proximityFun = NULL,
                      proximityType = c("individual", "population", "both")) {
  NULL
}
