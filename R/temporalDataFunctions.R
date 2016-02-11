##' Summarize a mating scene
##'
##' Create a summary of data contained within a matingScene data frame.
##'
##' @param popn a matingScene data frame or list
##' @param type character. whether to do a temporal (t), spatial (s), or mating
##' type (mt) summary. The default is "auto" which will automatically summarize
##' all mating information in popn
##' @param k integer. which nearest neighbor to calculate (only for type == "s")
##' @return a list or a list of lists containing summary information
##' including:\cr
##' temporal - year (year), population start date (popSt), mean individual start date
##' (meanSD), standard deviation of start (sdSD), mean duration (meanDur),
##' standard deviation of duration (sdDur), peakDay - day on which highest
##' number of individuals were receptive (peak), mean end date (meanED),
##' standard deviation of end date (sdED), population end date (popEnd)\cr
##' spatial - minimum x (minX), minimum y (minY), maximum x (maxX),
##' maximum y (maxY), average distance to kth nearest neighbor as specified
##' by k (k<n> where n is the input for k)\cr
##' compatibility - number of mating types (nMatType), average number of
##' compatible mates (meanComp)\cr
##' If popn is a multi-year mating scene, then the output will be a list
##' of lists, otherwise the output will be a single list
matingSummary <- function(popn, type = "auto", k = 1) {
  if (is.list(popn) & !is.data.frame(popn)) {
    matSum <- lapply(popn, matingSummary)
  } else {
    type <- match.arg(type, c("auto", "t", "s", "mt"))
    matSum <- list()
    if (type == "auto") {
      temp <- attr(popn, "t")
      spat <- attr(popn, "s")
      comp <- attr(popn, "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if (type == "t") {
        temp <- T
      } else if (type == "s") {
        spat <- T
      } else if (type == "mt") {
        comp <- T
      }
    }
    if (temp) {
      org <- attr(popn, "origin")
      matSum$year <- as.numeric(format(org, "%Y"))
      matSum$popSt <- as.Date(1, org)
      matSum$meanSD <- as.Date(mean(popn$start), org)
      matSum$sdSD <- sd(popn$start)
      matSum$meanDur <- mean(popn$duration)
      matSum$sdDur <- sd(popn$duration)
      matSum$peak <-
        as.Date(as.integer(which.max(colSums(receptivityByDay(popn)))), org)
      matSum$meanED <- as.Date(mean(popn$end), org)
      matSum$sdED <- sd(popn$end)
      matSum$popEnd <- as.Date(max(popn$end), org)
    }
    if (spat) {
      matSum$minX <- min(popn$x)
      matSum$minY <- min(popn$y)
      matSum$maxX <- max(popn$x)
      matSum$maxY <- max(popn$y)
      matSum[[paste("k", k, sep = "")]] <-
        mean(kNearNeighbors(popn, k)[k])
    }
    if (comp) {
      matSum$nMatType <- length(union(levels(popn$s1), levels(popn$s2)))
      popn$s1 <- as.character(popn$s1)
      popn$s2 <- as.character(popn$s2)
      s <- numeric(nrow(popn))
      for (i in 1:(nrow(popn)-1)) {
        for (j in (i+1):nrow(popn)) {
          same1 <- popn[i, "s1"] == popn[j, "s1"] & popn[i, "s2"] == popn[j, "s2"]
          same2 <- popn[i, "s1"] == popn[j, "s2"] & popn[i, "s2"] == popn[j, "s1"]
          if (!(same1 | same2)) {
            s[i] <- s[i]+1
            s[j] <- s[j]+1
          }
        }
      }
      matSum$meanComp <- mean(s)
    }
  }
  matSum
}

##' Calculate bootstrap confidence intervals for
##'
##' A great function
##'
##' This function reads draws lines in desirable places
##'
##' @param fs a flowering schedule or 3dpop object.
##' @param nboot integer number of times to resample FS.
##' @param bs.ci numeric vector of probabilities with values in [0,1].
##' @param returnDF logical indicating whether to return dataframe of all resampled parameters.
##' @export
##' @author Stuart Wagenius
##' @examples
##' \dontrun{bootstrapFS(NULL)}
##'
bootstrapFS <- function(fs, nboot = 10, bs.ci = c(0.025, 0.975), returnDF = FALSE){
  df.size <- dim(fs$df)[1]
  tmp <- fs
  d <- rep(as.Date("2013-01-01"), nboot)
  bs <- data.frame(peakDate = d, firstDay = d, lastDay = d, range50.1 = d, range50.2 = d)
  for(i in 1: nboot){
    tmp$df <- fs$df[sample(1:df.size, replace = TRUE), ]
    sfs <- summaryFS(tmp)
    bs$peakDate[i] <- sfs$peakDate
    bs$firstDay[i] <- sfs$opening
    bs$lastDay[i]  <- sfs$closing
    bs$range50.1[i]  <- sfs$range50[1]
    bs$range50.2[i]  <- sfs$range50[2]
  }
  y <- apply(bs, 2, probs = bs.ci, type = 1, FUN = quantile)
  w <- attributes(y)$dimnames
  attributes(y)$dimnames <- NULL
  y <- data.frame(y, stringsAsFactors= FALSE)
  ci <- data.frame(lapply(y, as.Date))
  row.names(ci) <- w[[1]]
  colnames(ci) <- w[[2]]
  ans <- list(CI = ci, nboot = nboot, n = df.size)
  if(returnDF) ans <- list(bootstrapDF = df, CI = ci, nboot = nboot, n = df.size)
  ans
}

##' Get comparisons of mating timing between all pairs
##'
##' @title Pairwise mating timing comparison
##' @param popn a 3D population object
##' @param overlapOrTotal whether to calculate the number of days that each
##' pair was overlapping in mating receptivity or the total number of days
##' that either individual was receptive
##' @param compareToSelf whether or not to include self comparisons in the
##' return value
##' @return a matrix containing all pairwise comparisons. If compareToSelf
##' is FALSE then there will be n rows and n-1 columns and indexing
##' result[i,j] where j > i must be done with result[i,j-1], whether result
##' is the return value of overlap. There is one attribute "idOrder" which
##' gives the order of the id column in popn at the time of the function call.
##' This can be useful to find certain elements in the matrix, see examples.
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' pop <- pop[order(pop$start),]
##' daysSync <- overlap(pop)
##' indices <- which(attr(daysSync, "idOrder") %in% c(1, 4))
##' if (indices[1] <= indices[2]) {
##'   daysSync[indices[1], indices[2]]
##' } else {
##'   daysSync[indices[1], indices[2]-1]
##' }
overlap <- function(popn, overlapOrTotal = c("overlap", "total"),
                    compareToSelf = FALSE) {
  n <- nrow(popn)
  startV <- popn$start
  endV <- popn$end
  overlapOrTotal <- match.arg(overlapOrTotal)
  # see src/temporalLoops.cpp for c++ code
  if (overlapOrTotal == "overlap") {
    if (compareToSelf) {
      overlapMatrix <- daysSync_self(startV, endV, n)
    } else {
      overlapMatrix <- daysSync_noself(startV, endV, n)
    }
  } else if (overlapOrTotal == "total") {
    if (compareToSelf) {
      overlapMatrix <- daysEither_self(startV, endV, n)
    } else {
      overlapMatrix <- daysEither_noself(startV, endV, n)
    }
  }
  attr(overlapMatrix, "idOrder") <- popn$id
  overlapMatrix
}


##' Create a matrix showing which individuals are flowering on a given day
##'
##' @title Flowering by day in a populations
##' @param popn a 3D population object
##' @return a matrix where the columns represent all mating days and the rows
##' represent all individuals in the population. The value for day i and
##' individual j will be TRUE if that individual was flowering on that day and
##' is located in position [i,j]
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene(size = 10)
##' receptivityByDay(pop)
receptivityByDay <- function(popn) {
  # get ids and days that flowering occurred
  ids <- popn$id
  days <- seq(min(popn$start), max(popn$end), 1)
  nID <- length(ids)
  nDay <- length(days)

  dailyMatrix <- matrix(F, nrow = nID, ncol = nDay)
  # for a given individual, say what days it was flowering on
  for (i in 1:nID) {
    dailyMatrix[i, popn[i, "start"]:popn[i, "end"]] <- T
  }

  rownames(dailyMatrix) <- ids
  colnames(dailyMatrix) <- days

  dailyMatrix
}

##' Calculate one of a variety of measures of mating synchrony for a population.
##'
##' Measures of synchrony are based on methods described in Augspurger (1983),
##' Kempenaers (1983), and from Wagenius (personal observations), as well
##' as variations on different factors of those measures.
##'
##' @title Mating synchrony of a population
##' @param popn a 3dPop object that has the flowering schedule for the
##' population of interest.
##' @param method character, partial matching allowed, describing what type
##' of synchrony will be calculated. "augspurger" is based on the method
##' described in Augspurger (1983). "kempenaers" is based on the method
##' described in Kempenaers (1993). "sync/either" will calculate a synchrony
##' value based on the number of days both individuals were flowering divided
##' by the number of days either individual was available for mating. "sync_nn"
##' gives the average of the kth nearest neighbor, or rather the kth most
##' synchronous individual.
##' @param synchronyType one of "population", "pairwise", "individual", or "all"
##' - see Value for more details.
##' @param averageType character. Identifies whether to take the mean or median
##' when calculating averages
##' @param syncNN integer. The kth nearest neighbor to be averaged when
##' calculating population synchrony
##' @param compareToSelf logical. Whether or not to include self comparisons
##' when calculation synchrony. Defaults to FALSE.
##' @return The result depends on the input for \code{synchronyType}. If
##' \code{synchronyType} is "population" \code{synchrony} will return a numeric
##' value that has a range depending on the \code{method}. If
##' \code{synchronyType} is "pairwise" \code{synchrony} will return a matrix
##' with all pairwise synchrony comparisons. It is important to note two things:
##' [1] if \code{method} is set to "sync_nn" then the pairwise comparisons will
##' be in descending order and cannot be indexed by ID order. [2] if
##' \code{compareToSelf} is set to FALSE, the matrix will have dimensions 100
##' rows by 99 columns. Similar to \code{\link{overlap}}, indexing will be
##' affected. If \code{synchronyType} is "individual" \code{synchrony} will
##' returns a data frame with a row for id and a row for individual synchrony.
##' If \code{synchronyType} is "all" \code{synchrony} will return a list
##' containing the values described above for population, pairwise, and
##' individual synchrony.
##' @export
##' @author Danny Hanson
##' @references Kempenaers, B. (1993) The use of a breeding synchrony index.
##' \emph{Ornis Scandinavica}, \strong{24}, 1. \cr\cr
##' Augspurger, C.K. (1983) Phenology, flowering synchrony, and fruit set of
##' six neotropical shrubs. \emph{Biotropica} \strong{15}, 257-267.
##' @examples
##' pop <- simulateScene(size = 150)
##' synchrony(pop, "augs")
##'
##' pop2 <- simulateScene(size = 1234, sdDur = 5, sk = 1)
##' syncVals <- synchrony(pop2, "sync_nn", "all", "median", 123)
synchrony <- function(popn, method, synchronyType = "all", averageType = "mean",
                      syncNN = 1, compareToSelf = FALSE) {

  method <- match.arg(method, c("augspurger", "kempenaers", "overlap",
                                "sync_nn"))
  synchronyType <- match.arg(synchronyType, c("population", "pairwise",
                                              "individual", "all"),
                             several.ok = T)
  averageType <- match.arg(averageType, c("mean", "median"))

  # some things that all methods need
  n <- nrow(popn) # population size
  if (averageType == "mean") {
    average <- mean
  } else if (averageType == "median") {
    average <- median
  }
  # deal with pop size and transposing matrices
  if (n < 2) {
    stop("Can't calculate synchrony for population size less than 2")
  }

  if (method == "augspurger") {
    syncMatrix <- overlap(popn, "overlap", compareToSelf = T)
    pairSync <- syncMatrix/popn$duration

    syncMatrix2 <- overlap(popn, "overlap", compareToSelf)
    pairSync2 <- syncMatrix2/popn$duration

    indSync <- data.frame(id = popn$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      average(pairSync2[i,])
    })

    popSync <- average(indSync[,2])

  } else if (method == "kempenaers") {
    indDaily <- receptivityByDay(popn)

    pairSync <- NULL

    indSync <- data.frame(id = popn$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      inds <- as.character(popn[i, "start"]:popn[i, "end"])
      sum(indDaily[,inds]) - popn[i,"duration"]
    })/(popn[,"duration"]*(n-1))

    popSync <- average(indSync[,2])

  } else if (method == "overlap") {
    syncMatrix <- overlap(popn, "overlap", compareToSelf = T)
    eitherMatrix <- overlap(popn, "total", compareToSelf = T)
    pairSync <- syncMatrix/eitherMatrix

    syncMatrix2 <- overlap(popn, "overlap", compareToSelf)
    eitherMatrix2 <- overlap(popn, "total", compareToSelf)
    pairSync2 <- syncMatrix2/eitherMatrix2

    indSync <- data.frame(id = popn$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      average(pairSync2[i,])
    })

    popSync <- average(pairSync)

  } else if (method == "sync_nn") {
    if (syncNN >= n) {
      stop("syncNN must be less than n")
    }

    syncMatrix <- overlap(popn, "overlap", compareToSelf = T)
    eitherMatrix <- overlap(popn, "total", compareToSelf = T)
    pairSyncInit <- syncMatrix/eitherMatrix

    syncMatrix2 <- overlap(popn, "overlap", compareToSelf)
    eitherMatrix2 <- overlap(popn, "total", compareToSelf)
    pairSyncInit2 <- syncMatrix2/eitherMatrix2

    m <- ifelse(compareToSelf, n, n-1)
    pairSync <- matrix(nrow = n, ncol = n)
    pairSync2 <- matrix(nrow = n, ncol = m)
    for (i in 1:n) {
      pairSync[i,] <- sort(pairSyncInit[i,], decreasing = T)
      pairSync2[i,] <- sort(pairSyncInit2[i,], decreasing = T)
    }

    indSync <- data.frame(id = popn$id, synchrony = -1)
    indSync$synchrony <- pairSync[,syncNN]

    popSync <- average(pairSync[,syncNN])

  }

  # return
  potential <- list()
  if ("population" %in% synchronyType) {
    potential$pop <- popSync
  }
  if ("individual" %in% synchronyType) {
    potential$ind <- indSync
  }
  if ("pairwise" %in% synchronyType) {
    potential$pair <- pairSync
  }
  if ("all" %in% synchronyType) {
    potential$pop <- popSync
    potential$ind <- indSync
    potential$pair <- pairSync
  }
  attr(potential, "t") <- TRUE
  attr(potential, "s") <- FALSE
  attr(potential, "c") <- FALSE
  potential
}



