##' simulateScene generates a matingScene object -- a simulated population
##' in a standard format with individuals randomly assigned a mating schedule,
##' a location, and S-alleles
##'
##' @title simulate a 3d population
##' @param size integer number of plants
##' @param meanSD date mean start date
##' @param sdSD date standard deviation of start date
##' @param skSD skew of the start date of the population
##' @param meanDur numeric duration in days
##' @param sdDur standard deviation of duration in days
##' @param xRange range of spatial extent of individuals along x-axis
##' @param yRange range of spatial extent of individuals along y-axis
##' @param distro unimplemented
##' @param sAlleles integer count of S-Alleles that could be in the population
##'
##' @return matingScene object -- see \code{\link{makeScene}}
##' @seealso \code{\link{makeScene}}
##' @author Stuart Wagenius
##' @examples
##' simulateScene()
##' \dontrun{simulateScene(NULL)}
simulateScene <- function(size = 30, meanSD = "2012-07-12", sdSD = 6, meanDur = 11,
                        sdDur = 3, skSD = 0 ,xRange = c(0, 100), yRange = c(0, 100),
                        distro = "unif", sAlleles = 10) {
  md <- as.integer(as.Date(meanSD, "%Y-%m-%d"))
  sd <- as.integer(md + round(sn::rsn(n = size, 0, omega = sdSD, alpha = skSD), 0))
  ed <- as.integer(sd + abs(round(rnorm(size, meanDur, sdDur), 0)))

  if (distro != "unif")
    warning("distro must be unif")
  xv <- runif(size, min = xRange[1], max = xRange[2])
  yv <- runif(size, min = yRange[1], max = yRange[2])
  sM <- sample(x = 1:sAlleles, size = size, replace = TRUE)
  sP <- sapply(sM, FUN = function(x) sample((1:sAlleles)[-x], 1))
  df <- data.frame(id = 1:size, start = sd, end = ed, x = xv,
                   y = yv, s1 = sM, s2 = sP)
  makeScene(df, startCol = "start", endCol = "end", idcode = "pla",
         dateFormat = "1970-01-01")
}

##' Turns a data frame with information about temporal, spatial, or
##' genetic mating data into a matingScene object using a standard format.
##'
##' @title Create a matingScene object from a data frame
##' @param df a data frame containing mating information
##' @param startCol character name of column with start dates
##' @param endCol character name of column with end dates
##' @param xCol character name of column with x or E coordinates
##' @param yCol character name of column with y or N coordinates
##' @param s1Col character name of one column with S-allele
##' @param s2Col character name of another column with S-alleles
##' @param idcode character name for column with unique identifier
##' @param dateFormat character giving either (1) the format of the start and end
##' date columns if those columns are characters or (2) the origin for the start
##' and end date columns if those columns are numeric. It is used in as.Date
##'
##' @return a matingScene object. A data frame with columns identified with
##' attributes for each of the column names. See details for more information.
##' @details
##' @author Danny Hanson
##' @examples
##' \dontrun{makeFS(NULL)}
makeScene <- function (df, startCol = "startDate", endCol = "endDate", xCol = "x",
                    yCol = "y", s1Col = "s1", s2Col = "s2", idcode = "id",
                    dateFormat = "%Y-%m-%d") {
  if (!idcode %in% names(df)) {
    df[, idcode] <- 1:nrow(df)
  }

  attr(df, "t") <- FALSE
  attr(df, "s") <- FALSE
  attr(df, "c") <- FALSE
  attr(df, "id") <- idcode

  if (all(c(startCol, endCol) %in% names(df))) {
    attr(df, "t") <- TRUE
    df[, startCol] <- as.integer(as.Date(df[, startCol], dateFormat))
    df[, endCol] <- as.integer(as.Date(df[, endCol], dateFormat))
    df[, "duration"] <- df[, endCol] - df[, startCol]
    origin <- ifelse(grepl("[0-9]", dateFormat), dateFormat, "1970-01-01")

    attr(df, "start") <- startCol
    attr(df, "end") <- endCol
    attr(df, "duration") <- "duration"
    attr(df, "origin") <- origin
  }

  if (all(c(xCol, yCol) %in% names(df))) {
    attr(df, "s") <- TRUE
    attr(df, "x") <- xCol
    attr(df, "y") <- yCol
  }

  if (all(c(s1Col, s2Col) %in% names(df))) {
    attr(df, "c") <- TRUE
    attr(df, "s1") <- s1Col
    attr(df, "s2") <- s2Col
  }

  df
}
