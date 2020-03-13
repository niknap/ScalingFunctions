# Variance of mixed distribution
var_mixdistr <- function(means, vars, weights=NA){
  if(is.na(weights)){
    weights <- numeric(length(means))
    weights[] <- 1
  }
  weights <- weights/sum(weights)
  return(sum(weights*(means^2+vars-mean.mix(means, weights)^2)))
}
