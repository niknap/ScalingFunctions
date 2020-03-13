# Mean of mixed distribution
mean_mixdistr <- function(means, weights=NA){
  if(is.na(weights)){
    weights <- numeric(length(means))
    weights[] <- 1
  }
  weights <- weights/sum(weights)
  return(sum(weights*means))
}
