#' Mean of mixture distribution
#'
#' Calculation of the mean of a mixture distribution from means of all contributing distributions
#' @param means Vector of means of all contributing distributions
#' @param weights Vector of weights of all contributing distributions
#' @return Arithmetic mean of mixture distribution
#' @keywords mean average mixture distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

mean_mixture <- function(means, weights=NA){
  if(is.na(weights[1])){
    weights <- numeric(length(means))
    weights[] <- 1
  }
  weights <- weights/sum(weights)
  result <- sum(weights*means)
  names(result) <- "mean"
  return(result)
}

