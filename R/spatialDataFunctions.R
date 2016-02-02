##' Compute all pairwise distances for a population. This function
##' is simply a wrapper for \code{dist} that only returns a matrix
##'
##' @title Distance matrix for a population
##' @param popn a 3D population object
##' @return a vector representing the lower triangle of the distance matrix
##' @export
##' @seealso \code{\link{dist}}
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' distance <- pairDist(pop)
pairDist <- function(popn) {
  as.matrix(dist(popn[, c("x", "y")]))
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
##' pop <- simulateScene(10)
##' kNearNeighbors(pop, 3)
kNearNeighbors <- function(popn, k) {
  knnMatrix <- FNN::knn.dist(popn[c("x", "y")], k = k, algorithm = "brute")
  rownames(knnMatrix) <- popn$id
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
##' pop <- simulateScene()
##' proximity(pop)
proximity <- function(popn, proximityFun = NULL,
                      proximityType = c("individual", "population", "both")) {
  NULL
}
