##' generatePop generates a 3dpop object -- a simulated population in a standard format with individuals randomly assigned a mating schedule, a random location, and S-alleles
##'
##'
##' A great function
##'
##' @title simulate a 3d population
##' @param size integer number of plants
##' @param meanSD date mean start date
##' @param sdSD date standard deviation of start date
##' @param sk skew of the start date of the population
##' @param meanDur numeric duration in days
##' @param sdDur standard deviation of duration in days
##'
##' @param xRange range of spatial extent of individuals along x-axis
##' @param yRange range of spatial extent of individuals along y-axis
##' @param distro unimplemented
##'
##' @param sAlleles integer count of S-Alleles that could be in the population
##'
##'
##' @return 3dpop object -- a data frame with columns identified
##' @export
##' @author Stuart Wagenius
##' @examples
##' generatePop()
##' \dontrun{generatePop(NULL)}
generatePop <- function(size = 30, meanSD = "2012-07-12", sdSD = 6, meanDur = 11,
                        sdDur = 3, sk = 0 ,xRange = c(0, 100), yRange = c(0, 100),
                        distro = "unif", sAlleles = 10) {
  md <- as.integer(as.Date(strptime(meanSD, "%Y-%m-%d")))
  md <- as.integer(md)
  sd <- as.integer(md + round(sn::rsn(n = size, 0, omega = sdSD, alpha = sk), 0))
  ed <- as.integer(sd + abs(round(rnorm(size, meanDur, sdDur), 0)))

  if (distro != "unif")
    warning("distro must be unif")
  xv <- runif(size, min = xRange[1], max = xRange[2])
  yv <- runif(size, min = yRange[1], max = yRange[2])
  sM <- sample(x = 1:sAlleles, size = size, replace = TRUE)
  sP <- sapply(sM, FUN = function(x) sample((1:sAlleles)[-x],
                                            1))
  df <- data.frame(pla = 1:size, start = sd, end = ed, x = xv,
                   y = yv, s1 = sM, s2 = sP)
  make3d(df, startCol = "start", endCol = "end", idcode = "pla",
         dateFormat = "1970-01-01")
}

##' generateFS generates a random flowering schedule with standard format
##'
##' A great function
##'
##' @title simulate a flowering schedule
##' @param size integer number of plants
##' @param meanSD date mean start date
##' @param sdSD date standard deviation of start date
##' @param sk skew of the start date of the population
##' @param meanDur numeric duration in days
##' @param sdDur standard deviation of duration in days
##' @return data frame with startCol endCol added
##' @export
##' @author Stuart Wagenius
##' @examples
##' generateFS()
##' \dontrun{generateFS(NULL)}
generateFS <- function(size = 30, meanSD = "2012-07-12", sdSD = 6, meanDur = 11,
                       sdDur = 5, sk = 0)  {
  md <- as.Date(strptime(meanSD, "%Y-%m-%d"))
  sd <- md + round(sn::rsn(n = size, 0, omega = sdSD, alpha = sk), 0)
  ed <- sd + abs(round(rnorm(size, meanDur, sdDur), 0))
  fs <- data.frame(pla = 1:size, start = sd, end = ed)
  makeFS(fs, startCol = "start", endCol = "end", dateFormat = "1970-01-01")
}

##' make3d edits a dataframe into a 3dpop object with standard format
##'
##' A great function
##'
##' @title update a data frame to a 3dpop
##' @param df data frame
##' @param startCol character name of column with start dates
##' @param endCol character name of column with end dates
##' @param xCol character name of column with x or E coordinates
##' @param yCol character name of column with y or N coordinates
##' @param s1Col character name of one column with S-allele
##' @param s2Col character name of another column with S-alleles
##' @param idcode character name for column with unique identifier
##' @param dateFormat character giving either the format of the start and end
##' dates columns if those columns are characters or the origin if those
##' columns are numeric (used in as.Date)
##'
##' @return 3dpop object, a data frame with columns identified
##' @export
##' @author Stuart Wagenius
##' @examples
##' \dontrun{makeFS(NULL)}
make3d <- function (df, startCol = "startDate", endCol = "endDate", xCol = "x",
                    yCol = "y", s1Col = "s1", s2Col = "s2", idcode = "id",
                    dateFormat = "%Y-%m-%d") {
  if (!idcode %in% names(df))
    df[, idcode] <- 1:dim(df)[1]
  df[, startCol] <- as.integer(as.Date(df[, startCol], dateFormat))
  df[, endCol] <- as.integer(as.Date(df[, endCol], dateFormat))
  ans <- list(df = df, start = startCol, end = endCol, x = xCol,
              y = yCol, s1 = s1Col, s2 = s2Col, id = idcode)
  ans
}

##' makeFS edits a dataframe into a flowering schedule with standard format
##'
##' A great function
##'
##' @title update a data frame to a flowernig schedule
##' @param df data frame
##' @param startCol character name of column with start dates
##' @param endCol character name of column with end dates
##' @param dateFormat character giving either the format of the start and end
##' dates columns if those columns are characters or the origin if those
##' columns are numeric (used in as.Date)
##' @return data frame with startCol endCol added
##' @export
##' @author Stuart Wagenius
##' @examples
##' flowers <- data.frame(id = c("one", "two"),
##'                       start = c("2015/07/24", "2015/08/05"),
##'                       end = c("2015/08/08", "2015/08/11"))
##' makeFS(flowers, "start", "end", "%Y/%m/%d")
##' \dontrun{makeFS(NULL)}
makeFS <- function(df, startCol= "startDate", endCol= "endDate", dateFormat = "%Y-%m-%d") {
  df[, startCol] <- as.integer(as.Date(df[, startCol], dateFormat))
  df[, endCol] <- as.integer(as.Date(df[, endCol], dateFormat))
  ans <- list(df = df, start = startCol, end = endCol)
  ans
}

##' Get the element out of a dist object or dist-like vector that would be
##' in position [i,j] if the dist object was a matrix Depending on whether
##' the diagonal is included, the formula to get the [i,j] element is
##' n*(i-1)-i*(i-1)/2+j if \code{diag} is TRUE or n*(i-1)-i*(i+1)/2+j
##' if \code{diag} is FALSE.
##'
##' @title Get elements from a dist object
##' @param distO a dist object or vector made in a dist-like manner
##' @param i integer, the row of the object of interest
##' @param j integer, the column of the object of interest
##' @param diag logical, whether or not the diagonal has been included
##' in the dist-like vector.
##' @return the element that would be in position [i,j] if the dist object
##' was a matrix
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop(size = 200)
##' daySync <- daysSynchronous(pop)
##' getDistij(daySync, 20, 30)
##'
##' daySyncSelf <- daysSynchronous(pop, TRUE)
##' getDistij(daySync, 20, 30, TRUE)
getDistij <- function(distO, i, j, diag = FALSE) {
  # get size of square matrix (n)
  firstN <- sqrt(1+8*length(distO))/2
  n <- ifelse(diag, firstN-1/2, firstN+1/2)
  # check if n is a good number
  if (!round(n) == n) {
    warning("distO is not a proper length. It should be n*(n+1)/2 where n is
            the number of rows/columns in the square distance matrix. Running
            code with calculated n rounded to nearest integer")
    n <- round(n)
  }
  # switch anything where i > j
  ij <- expand.grid(i,j)
  if (!diag) {
    ij <- ij[ij[,1] != ij[,2],] # remove i == j if !diag
  }
  inds <- ij[,1] > ij[,2]
  tmp <- ij[inds,1]
  ij[inds,1] <- ij[inds,2]
  ij[inds,2] <- tmp

  # return the appropriate values based on calculated indices
  if (diag) {
    # n*(i-1)-i*(i-1)/2+j
    retVal <- distO[n*(ij[,1]-1) - ij[,1]*(ij[,1]-1)/2 + ij[,2]]
  } else {
    # n*(i-1)-i*(i+1)/2+j
    retVal <- distO[n*(ij[,1]-1) - ij[,1]*(ij[,1]+1)/2 + ij[,2]]
  }
  retVal
}

##' Get i and j given the index within a dist object or dist-like vector
##'
##' @title Get i and j from a dist index
##' @param ind the index within the dist-like vector
##' @param n the population size. You need either this or distO
##' @param distO a dist object or vector made in a dist-like manner
##' @param diag logical, whether or not the diagonal has been included
##' in the dist-like vector.
##' @return a vector (i, j) of the indeces of the individuals from the original
##' population
##' @export
##' @author Danny Hanson
##' @examples
##' pop <- generatePop(size = 200)
##' dayEither <- daysEitherFlowering(pop)
##' highOnes <- which(dayEither > 10)
##' getIndFromij(highOnes, 200)
##' getIndFromij(highOnes, distO = dayEither) # should be equal
getIndFromij <- function(ind, n = -1, distO = NULL, diag = FALSE) {
  # get size of square matrix (n)
  if (n == -1) {
    if (!is.null(distO)) {
      firstN <- sqrt(1+8*length(distO))/2
      n <- ifelse(diag, firstN-1/2, firstN+1/2)
      # check if n is a good number
      if (!round(n) == n) {
        warning("distO is not a proper length. It should be n*(n+1)/2 where n is
            the number of rows/columns in the square distance matrix. Running
            code with calculated n rounded to nearest integer")
        n <- round(n)
      }
    } else {
      stop("You must enter a value for either n or distO")
    }
  } else {
    if (!(round(n) == n) | n < 1) {
      stop("n must be an integer > 0")
    }
  }

  # return the appropriate values based on calculated indices
  if (diag) {
    # n*(i-1)-i*(i-1)/2+j
    retVal <- distO[n*(ij[,1]-1) - ij[,1]*(ij[,1]-1)/2 + ij[,2]]
  } else {
    # n*(i-1)-i*(i+1)/2+j
    retVal <- distO[n*(ij[,1]-1) - ij[,1]*(ij[,1]+1)/2 + ij[,2]]
  }
  retVal
}
