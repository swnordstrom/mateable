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

##' Extract temporal columns from a 3d population object
##'
##' @title Get a data frame with only temporal data
##' @param popn a 3d population object
##' @return a data frame with only the id, start, and end columns from the
##' 3d population object
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' flowerSched <- getTemporalDF(pop)
getTemporalDF <- function(popn) {
  id <- popn$df[[popn$id]]
  start <- popn$df[[popn$start]]
  end <- popn$df[[popn$end]]
  data.frame(id = id, start = start, end = end)
}

##' Get the duration for each individual in the population
##'
##' @title duration for each individual
##' @param popn a 3D population object
##' @return a data frame with IDs and durations
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' getDuration(pop)
getDuration <- function(popn) {
  dur <- popn$df[[popn$end]] - popn$df[[popn$start]] + 1
  dur <- data.frame(id = popn$df[[popn$id]], duration = dur)
  dur
}

##' Get comparisons of mating timing overlap between all pairs
##'
##' @title Pairwise mating timing overlap
##' @param popn a 3D population object
##' @param compareToSelf whether or not to include self comparisons in the
##' return value
##' @return a dist-like vector that represents the matrix of
##' pairwise comparisons
##' @seealso \code{\link{getDistij}}
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' daySync <- daysSynchronous(pop)
##' getDistij(daySync, 12, 34)
daysSynchronous <- function(popn, compareToSelf = FALSE) {
  n <- nrow(popn$df)
  startV <- popn$df[,popn$start]
  endV <- popn$df[,popn$end]
  # see matential/temporalLoops.cpp for sync_loop code
  if (compareToSelf) {
    syncMatrix <- sync_loop_diag(startV, endV, n)
  } else {
    syncMatrix <- sync_loop_nodiag(startV, endV, n)
  }
  attr(syncMatrix, "n") <- n
  attr(syncMatrix, "includeSelf") <- compareToSelf
  attr(syncMatrix, "indices") <- popn$df[[popn$id]]
  syncMatrix
}


##' Get all pairwise comparisons of how many days either individual was
##' flowering
##'
##' @title Pairwise total mating timing
##' @param popn a 3D population object
##' @param compareToSelf whether or not to include self comparisons in the
##' return value
##' @return a dist-like vector that represents the matrix of
##' pairwise comparisons
##' @seealso \code{\link{getDistij}}
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' daySync <- daysEitherFlowering(pop)
##' getDistij(daySync, 12, 34)
daysEitherFlowering <- function(popn, compareToSelf = FALSE) {
  n <- nrow(popn$df)
  startV <- popn$df[,popn$start]
  endV <- popn$df[,popn$end]
  # see matential/temporalLoops.cpp for either_loop code
  if (compareToSelf) {
    eitherMatrix <- either_loop_diag(startV, endV, n)
  } else {
    eitherMatrix <- either_loop_nodiag(startV, endV, n)
  }
  attr(eitherMatrix, "n") <- n
  attr(eitherMatrix, "includeSelf") <- compareToSelf
  attr(eitherMatrix, "indices") <- popn$df[[popn$id]]
  eitherMatrix
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
##' pop <- generatePop(size = 10)
##' individualDailyFlowering(pop)
individualDailyFlowering <- function(popn) {
  # get ids and days that flowering occurred
  ids <- popn$df[, popn$id]
  days <- seq(min(popn$df[, popn$start]), max(popn$df[, popn$end]), 1)
  nID <- length(ids)
  nDay <- length(days)
  firstDayPop <- min(popn$df[, popn$start])

  # for a given individual, say what days it was flowering on
  makeDayRow <- function(i) {
    startInd <- min(popn$df[i, popn$start]) - firstDayPop + 1
    endInd <- max(popn$df[i, popn$end]) - firstDayPop + 1
    dayRow <- rep(F, nDay) # initialize days as false
    dayRow[startInd:endInd] <- T # for days it was flowering, set to true
    dayRow
  }
  dailyMatrix <- t(sapply(1:nID, makeDayRow))
  rownames(dailyMatrix) <- ids
  colnames(dailyMatrix) <- days

  dailyMatrix
}

##' Get the start and end flowering days of an individual.
##'
##' @title Get flowering days of an individual
##' @param popn a 3D population object
##' @param ind integer. The index/indices of the individual(s) to examine
##' @return a vector containing the start and end days relative to the first
##' day of flowering of the individual(s)
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop()
##' getDaysFlowering(pop, 3)
getDaysFlowering <- function(popn, ind) {
  firstDayPop <- min(popn$df[, popn$start])
  startInd <- min(popn$df[ind, popn$start]) - firstDayPop + 1
  endInd <- max(popn$df[ind, popn$end]) - firstDayPop + 1
  c(startInd, endInd)
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
##' rows by 99 columns. Again, indexing will be affected. If \code{synchronyType}
##' is "individual" \code{synchrony} will returns a data frame with a row for
##' id and a row for individual synchrony. If \code{synchronyType} is "all"
##' \code{synchrony} will return a list containing the values described above for
##' population, pairwise, and individual synchrony.
##' @export
##' @author Danny Hanson
##' @references Kempenaers, B. (1993) The use of a breeding synchrony index.
##' \emph{Ornis Scandinavica}, \strong{24}, 1. \cr\cr
##' Augspurger, C.K. (1983) Phenology, flowering synchrony, and fruit set of
##' six neotropical shrubs. \emph{Biotropica} \strong{15}, 257-267.
##' @examples
##' pop <- generatePop(size = 150)
##' synchrony(pop)
##'
##' pop2 <- generatePop(size = 1234, sdDur = 5, sk = 1)
##' syncVals <- synchrony(pop2, "sync_nn", "all", "median", 123)
synchrony <- function(popn, method = c("augspurger", "kempenaers", "sync/either",
                                       "sync_nn"),
                      synchronyType = c("population", "pairwise", "individual",
                                        "all"), averageType = c("mean", "median"),
                      syncNN = 1, compareToSelf = FALSE) {
  method <- match.arg(method)
  synchronyType <- match.arg(synchronyType)
  averageType <- match.arg(averageType)

  # some things that all methods need
  durMatrix <- getDuration(popn)
  n <- nrow(durMatrix) # population size
  syncMatrix <- daysSynchronous(popn, compareToSelf) # 3 of 4 methods need this
  if (averageType == "mean") {
    average <- mean
  } else if (averageType == "median") {
    average <- median
  }
  # deal with pop size and transposing matrices
  if (n < 2) {
    stop("Can't calculate synchrony for population size less than 2")
  } else if (n == 2) {
    transpose <- function(a) { t(t(a)) }
  } else {
    transpose <- function(a) { t(a) }
  }

  if (method == "augspurger") {
    # NOTE: if compareToSelf != TRUE then this will only have 99 columns and
    # indexing pairSync[i,j] where j > i will be done by pairSync[i,j-1]
    pairSync <- transpose(sapply(1:n, FUN = function(i) {
      getDistij(syncMatrix, i, 1:n, diag = compareToSelf)/durMatrix[i,2]
    }))

    indSync <- data.frame(id = durMatrix$id, synchrony = -1)
    indSync$synchrony <- rowMeans(pairSync)

    popSync <- average(indSync[,2])
  } else if (method == "kempenaers") {
    indDaily <- individualDailyFlowering(popn)

    pairSync <- NULL

    indSync <- data.frame(id = durMatrix$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      sAndE <- getDaysFlowering(popn, i)
      sum(indDaily[,sAndE[1]:sAndE[2]]) - durMatrix[i,2]
    })/(durMatrix[,2]*(n-1))

    popSync <- average(indSync[,2])
  } else if (method == "sync/either") {
    eitherMatrix <- daysEitherFlowering(popn, compareToSelf)
    sByE <- syncMatrix/eitherMatrix

    # NOTE: if compareToSelf != TRUE then this will only have 99 columns and
    # indexing pairSync[i,j] where j > i will be done by pairSync[i,j-1]
    pairSync <- transpose(sapply(1:n, FUN = function(i) {
      getDistij(sByE, i, 1:n, diag = compareToSelf)
    }))

    indSync <- data.frame(id = durMatrix$id, synchrony = -1)
    indSync$synchrony <- sapply(1:n, FUN = function(i) {
      average(pairSync[i,])
    })

    popSync <- average(sByE)
  } else if (method == "sync_nn") {
    if (syncNN >= n) {
      stop("syncNN must be less than n")
    }

    eitherMatrix <- daysEitherFlowering(popn, compareToSelf)
    sByE <- syncMatrix/eitherMatrix

    # NOTE: if compareToSelf != TRUE then this will only have 99 columns and
    # indexing pairSync[i,j] where j > i will be done by pairSync[i,j-1]
    # also pairSync won't make much sense because it's being sorted
    pairSync <- transpose(sapply(1:n, FUN = function(i) {
      sort(getDistij(sByE, i, 1:n, diag = compareToSelf), T)
    }))

    indSync <- data.frame(id = durMatrix$id, synchrony = -1)
    indSync$synchrony <- pairSync[,syncNN]

    popSync <- average(pairSync[,syncNN])
  }

  # return
  switch(synchronyType,
         population = popSync,
         pairwise = pairSync,
         individual = indSync,
         all = list(pop = popSync, pair = pairSync, ind = indSync))
}
