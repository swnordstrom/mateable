dailyOMP <- function(scene, k = 4, days = min(scene$start):max(scene$end), alpha = 1/13, nn.constant = FALSE){
  out <- matrix(nrow  = nrow(scene), ncol = length(days), dimnames = list(scene$id, days))
  rbd <- receptivityByDay(scene)
  if(nn.constant){
    knn <- unlist(get.knn(scene[,c('x','y')], k = k))
    dim(knn) <- c(nrow(scene), k, 2)
    mat <- matrix(nrow = nrow(scene),ncol = nrow(scene))
    for (y in 1:nrow(scene)){
      mat[y,knn[y,,1]]<-exp(knn[y,,2, drop = FALSE]*-1*alpha)
    }
    mat[is.na(mat)]<- 0
  }
  for (i in 1:length(days)){
    day <- days[i]
    idsDi <- which(rbd[,day])
    if(!nn.constant){
      nbrs <- k
      if(length(idsDi) <= k){
        nbrs <- length(idsDi)-1
      }
      knn <- unlist(get.knn(scene[idsDi,c('x','y')], k = nbrs))
      dim(knn) <- c(length(idsDi), nbrs, 2)
      out[idsDi,i] <- rowSums(exp(knn[,,2, drop = FALSE]*-1*alpha))
      out[is.na(out)]<- 0
    } else {
      out <- mat %*% rbd
    }
  }
  out
}
