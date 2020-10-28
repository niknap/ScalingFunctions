#' Variance of mixture distribution
#'
#' Calculation of the variance of a mixture distribution from means and variances of all contributing distributions
#' @param means Vector of means of all contributing distributions
#' @param vars Vector of variances of all contributing distributions
#' @param weights Vector of weights of all contributing distributions
#' @return Variance of mixture distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

var_of_mixture <- function(means, vars, weights=NA){
  if(is.na(weights[1])){
    weights <- numeric(length(means))
    weights[] <- 1
  }
  weights <- weights/sum(weights)
  result <- sum(weights*(means^2+vars-mean_of_mixture(means, weights)^2))
  names(result) <- "var"
  return(result)
}


