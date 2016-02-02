##' summaryFS returns standard info about a flowering schedule
##'
##' A great function
##'
##' @title summary of a flowering schedule
##' @param fs, a data frame in flowering schedule format
##' @param opening optional date to specify first date of interest
##' @param closing optional date to specify last date of interest
##' @return a names list with info about the flowering schedule
##' @export
##' @author Stuart Wagenius
##' @examples
##' fs <- generateFS()
##' summaryFS(fs)
##' \dontrun{summaryFS(NULL)}
summaryFS <- function(fs, opening = NULL, closing = NULL) {
  if(is.null(opening)) opening <- as.Date(min(fs$df[, fs$start]), "1970-01-01")
  if(is.null(closing)) closing <- as.Date(max(fs$df[, fs$end]), "1970-01-01")
  count <- dim(fs$df)[1]
  season <- seq(opening, closing, 1)
  dailyCount <- integer(length(season))
  for (i in 1:length(season)){
    dailyCount[i] <- sum(season[i] >= fs$df[, fs$start] & season[i] <= fs$df[, fs$end])
  }
  fd <- data.frame(day = season, count = dailyCount)
  peak <- fd[mean(which.max(fd$count)), "day"]
  range50 <- c(as.Date(median(fs$df[ , fs$start]), "1970-01-01"),
               as.Date(median(fs$df[ , fs$end]), "1970-01-01"))
  ans <- list(opening = opening, closing = closing, peakDate = peak, range50 = range50, fl.density = fd)
  ans
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
##' indices <- which(attr(daysSync, "inOrder") %in% c(1, 4))
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
  firstDayPop <- min(popn$start)

  # for a given individual, say what days it was flowering on
  makeDayRow <- function(i) {
    startInd <- min(popn[i, "start"]) - firstDayPop + 1
    endInd <- max(popn[i, "end"]) - firstDayPop + 1
    dayRow <- rep(F, nDay) # initialize days as false
    dayRow[startInd:endInd] <- T # for days it was flowering, set to true
    dayRow
  }
  dailyMatrix <- t(sapply(1:nID, makeDayRow))
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
##' synchrony(pop)
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
    syncMatrix <- overlap(popn, "overlap", compareToSelf)
    pairSync <- syncMatrix/popn$duration

    indSync <- data.frame(id = popn$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      average(pairSync[i,])
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
    syncMatrix <- overlap(popn, "overlap", compareToSelf)
    eitherMatrix <- overlap(popn, "total", compareToSelf)
    pairSync <- syncMatrix/eitherMatrix

    indSync <- data.frame(id = popn$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      average(pairSync[i,])
    })

    popSync <- average(pairSync)

  } else if (method == "sync_nn") {
    if (syncNN >= n) {
      stop("syncNN must be less than n")
    }

    syncMatrix <- overlap(popn, "overlap", compareToSelf)
    eitherMatrix <- overlap(popn, "total", compareToSelf)
    pairSyncInit <- syncMatrix/eitherMatrix

    pairSync <- matrix(nrow = n, ncol = n-1)
    for (i in 1:n) {
      pairSync[i,] <- sort(pairSyncInit[i,], decreasing = T)
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



