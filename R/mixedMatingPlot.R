mixedMatingPlot <- function(popn, dimension = "auto",
                            opening = NULL, closing = NULL,
                            dailyPoints = FALSE, drawQuartiles = FALSE,
                            includeID = FALSE,
                            sub= NULL, xlab = 'xlab', ylab = 'ylab', pch = NULL,
                            ptSize = 1, segWt = 3,
                            quartileWt = 2,
                            quartileColor = 'gray81',
                            peakColor = 'gray27', ...){

  # if t, s, mt: map with points colored by DAM with lines connecting compatible mating types
  # if s,t : map with points colored by DAM
  # if t,s: flowering schedule with individuals' rows clustered by distance to one another?
  # if s, mt: map with lines connecting compatible mating types
  # if t, mt: flowering schedule with individual's rows clustered by mating type similarity????
  # if mt, t: mating type plot with a point for each individual (jittered) colored by DAM
  # if mt, s: same as s, mt....


  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  nm <- par("mar")
  nmfrow <- par('mfrow')
  noma <- par('oma')

  if (is.list(popn) & !is.data.frame(popn)) {
    if ("auto" %in% dimension) {
      temp <- attr(popn[[1]], "t")
      spat <- attr(popn[[1]], "s")
      comp <- attr(popn[[1]], "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if ("t" %in% dimension) {
        temp <- T
      }
      if ("s" %in% dimension) {
        spat <- T
      }
      if ("mt" %in% dimension) {
        comp <- T
      }
    }
    nr <- length(popn)
    par(mfrow = c(nr,1))
    par(oma = c(4,3,2,1))

    count <- nrow(popn[[1]])
    emin <- min(popn[[1]]['x'])
    emax <- max(popn[[1]]['x'])
    nmin <- min(popn[[1]]['y'])
    nmax <- max(popn[[1]]['y'])
    smin <- min(as.numeric(popn[[1]][['s1']]))
    smax <- min(as.numeric(popn[[1]][['s1']]))
    opening <- min(popn[[1]]['start'])
    closing <- max(popn[[1]]['end'])

    for (i in 1:length(popn)){
      if (nrow(popn[[i]]) > max(count)){
        count <- nrow(popn[[i]])
      }
      if (min(popn[[i]]['x'])< emin){
        emin <- min(popn[[i]]['x'])
      }
      if (max(popn[[i]]['x'])< emax){
        emax <- max(popn[[i]]['x'])
      }
      if (min(popn[[i]]['y'])< nmin){
        nmin <- min(popn[[i]]['y'])
      }
      if (max(popn[[i]]['y'])< nmax){
        nmax <- max(popn[[i]]['y'])
      }
      if (max(as.numeric(popn[[i]][['s1']])>smax)){
        smax <- max(as.numeric(popn[[i]][['s1']]))
      }
      if (max(as.numeric(popn[[i]][['s2']])>smax)){
        smax <- max(as.numeric(popn[[i]][['s2']]))
      }
      if (min(as.numeric(popn[[i]][['s1']])>smin)){
        smin <- min(as.numeric(popn[[i]][['s1']]))
      }
      if (min(as.numeric(popn[[i]][['s2']])>smin)){
        smin <- min(as.numeric(popn[[i]][['s2']]))
      }
      if (min(popn[[i]]['start']) < opening){
        opening <- min(popn[[i]]['start'])
      }
      if (max(popn[[i]]['end']) < closing){
        closing <- min(popn[[i]]['end'])
      }
    }



  } else {
    # popn is not a list
    if (temp){
      # calculate DAM for inds
    }
    if (comp){
      # create compatibility matrix
    }

    if (temp & spat & comp){
      # map with points colored by DAM, lines btw compatible mts
    }

    if (temp & spat){
      # map with points colored by DAM
    }

    if(spat & comp){
      # map with lines drawn between compatible mts
    }

    if (temp & comp){
      # allele plot with points colored by DAM
    }

  }

}
