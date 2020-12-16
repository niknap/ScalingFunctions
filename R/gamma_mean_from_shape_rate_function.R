#' Gamma mean from shape and rate parameter
#'
#' Calculation of the mean of a gamma distribution from its shape and rate
#' parameters
#' @param shape parameter of gamma distribution
#' @param rate parameter of gamma distribution
#' @return Mean of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_mean_from_shape_rate <- function(shape, rate){
  return(shape/rate)
}
