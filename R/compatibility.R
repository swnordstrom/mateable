# #### mating type compatibility ####

compatibility <- function(popn, method = "singleLocus", subject = "all", averageType = "mean", diallel, dominance){

  method <- match.arg(method, c("singleLocus"))
  subject <- match.arg(subject, c("population", "pairwise",
                                  "individual", "all"), several.ok = T)
  averageType <- match.arg(averageType, c("mean", "median"))

  if (averageType == "mean") {
    average <- mean
  } else if (averageType == "median") {
    average <- median
  }

  pairCompat <- pair_compat(popn$s1, popn$s2)

  indCompat <- data.frame(id = popn$id, compatibility = -1)
  indCompat$compatibility <- apply(pairCompat, 1, average, na.rm = T)
  popCompat <- average(indCompat$compatibility)

    # return
    potential <- list()
    if ("population" %in% subject) {
      potential$pop <- popCompat
    }
    if ("individual" %in% subject) {
      potential$ind <- indCompat
    }
    if ("pairwise" %in% subject) {
      potential$pair <- pairCompat
    }
    if ("all" %in% subject) {
      potential$pop <- popCompat
      potential$ind <- indCompat
      potential$pair <- pairCompat
    }
    attr(potential, "t") <- FALSE
    attr(potential, "s") <- FALSE
    attr(potential, "c") <- TRUE
    potential

}
