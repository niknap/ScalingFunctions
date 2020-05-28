#' Gamma shape parameter from mean and variance
#'
#' Calculation of the shape parameter of a gamma distribution from its
#' mean and variance
#' @param mean of gamma distribution
#' @param variance of gamma distribution
#' @return Shape parameter of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_shape_from_mean_var <- function(mean, var){
  shape <- mean^2/var
  names(shape) <- "shape"
  return(shape)
}
