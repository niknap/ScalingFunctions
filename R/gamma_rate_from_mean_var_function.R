#' Gamma rate parameter from mean and variance
#'
#' Calculation of the rate parameter of a gamma distribution from its
#' mean and variance
#' @param mean of gamma distribution
#' @param variance of gamma distribution
#' @return Rate parameter of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_rate_from_mean_var <- function(mean, var){
  rate <- mean/var
  names(rate) <- "rate"
  return(rate)
}
