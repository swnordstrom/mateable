# a function to convert a mating summary list to a dataframe
matingSummary.df <- function(matSum){
  if(is.list(matSum[[1]][[1]])){
    df <- lapply(matSum, matingSummary.df)
  }
  if(is.list(matSum[[1]][1])){
    l <- lapply(matSum, unlist)
    df <- as.data.frame(do.call(rbind, l))
    df$popSt <- as.Date(df$popSt, origin = '1970-01-01')
    df$peak <- as.Date(df$peak, origin = '1970-01-01')
    df$meanSD <- as.Date(df$meanSD, origin = '1970-01-01')
    df$meanED <- as.Date(df$meanED, origin = '1970-01-01')
    df$popEnd <- as.Date(df$popEnd, origin = '1970-01-01')
    df$id <- rownames(df)
    if (!all(df$id == as.character(df$year))){
      df <- df[,c(ncol(df),1:(ncol(df)-1))]
    } else{
      df <- df[,1:(ncol(df)-1)]
    }
    rownames(df) <- NULL
  } else {
    df <- as.data.frame(matSum)
  }
  df
}


