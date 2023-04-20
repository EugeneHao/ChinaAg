get_lag <- function(data, oldname, lag, weight)
{
  newvec <- NULL
  index = which(names(data) == oldname)
  for(i in unique(data$region))
  {
    for(j in unique(data$crop))
    {
      tmpdat <- data %>% filter(region == i, crop == j)
      oldvec <- tmpdat[,index] %>% unlist()
      tmpvec <- NULL
      for(k in 1:lag)
      {
        tmpvec <- cbind(tmpvec, c(rep(oldvec[1], k), oldvec[-(22 - (1:k))]))
      }
      newvec <- c(newvec, as.vector(tmpvec %*% weight))
    }
  }
  return(newvec)
}
