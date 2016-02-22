##' Compute all pairwise distances for a population. This function
##' is simply a wrapper for \code{dist} that only returns a matrix
##'
##' @title Distance Matrix for a Population
##' @param popn a 3D population object
##' @return a matrix of all pairwise comparisons with attribute for order of
##' ids (idOrder)
##' @export
##' @seealso \code{\link{dist}}
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' distance <- pairDist(pop)
pairDist <- function(popn) {
  distMat <- as.matrix(dist(popn[, c("x", "y")]))
  attr(distMat, "idOrder") <- attr(distMat, "dimnames")[[1]]
  attr(distMat, "dimnames") <- NULL
  distMat
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

##' Calculate one of several of measures of mating synchrony for a population.
##'
##' @title Spatial Proximity of a Population
##' @param popn a 3D population object
##' @param method currently one of "maxProp", and "maxPropSqrd"
##' @param proximityFun a function used to calculate proximity
##' @param averageType whether to calculate individual and population proximity
##' using the mean or median
##' @param subject whether you want pair, individual, population, or all
##' @return NULL
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' proximity(pop)
proximity <- function(popn, method, proximityFun = NULL, averageType = "mean",
                      subject = "all") {
  method <- match.arg(method, c("maxProp", "maxPropSqrd"))
  n <- nrow(popn)
  distMatrix <- pairDist(popn)
  maxDist <- max(distMatrix)
  if (averageType == "mean") {
    average <- mean
  } else if (averageType == "median") {
    average <- median
  }
  # deal with pop size
  if (n < 2) {
    stop("Can't calculate proximity for population size less than 2")
  }

  if (method == "maxProp") {
    distNoDiag <- matrix(distMatrix[-seq(1, n^2, n+1)], nrow = n, byrow = T)
    pairProx <- 1 - distMatrix/maxDist
    pairProx2 <- 1 - distNoDiag/maxDist

    indProx <- data.frame(id = popn$id, proximity = -1)
    indProx$proximity <- sapply(1:n, FUN = function(i) {
      average(pairProx2[i,])
    })

    popProx <- average(indProx[,2])
  } else if (method == "maxPropSqrd") {
    distNoDiag <- matrix(distMatrix[-seq(1, n^2, n+1)], nrow = n, byrow = T)
    pairProx <- (1 - distMatrix/maxDist)^2
    pairProx2 <- (1 - distNoDiag/maxDist)^2

    indProx <- data.frame(id = popn$id, synchrony = -1)
    indProx$synchrony <- sapply(1:n, FUN = function(i) {
      average(pairProx2[i,])
    })

    popProx <- average(indProx[,2])
  }

  # return
  potential <- list()
  if ("population" %in% proximityType) {
    potential$pop <- popProx
  }
  if ("individual" %in% proximityType) {
    potential$ind <- indProx
  }
  if ("pairwise" %in% proximityType) {
    potential$pair <- pairProx
  }
  if ("all" %in% proximityType) {
    potential$pop <- popProx
    potential$ind <- indProx
    potential$pair <- pairProx
  }
  attr(potential, "t") <- FALSE
  attr(potential, "s") <- TRUE
  attr(potential, "c") <- FALSE
  potential
}
