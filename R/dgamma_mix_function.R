#' Density of mixed gamma distribution
#'
#' Probability density function for a mixture distribution of several gamma
#' distributions (all contributing with equal weights)
#' @param x Input vector of values
#' @param shape Vector of shape parameters of all contributing gamma
#' distributions
#' @param rate Vector of rate parameters of all contributing gamma
#' distributions
#' @return Vector of densities for each value in x
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

dgamma_mix <- function(x, shape, rate){
  d.vec <- dgamma(x=x, shape=shape[1], rate=rate[1]) / length(shape)
  for(i in 2:length(shape)){
    d.vec <- d.vec + dgamma(x=x, shape=shape[i], rate=rate[i]) / length(shape)
  }
  return(d.vec)
}
