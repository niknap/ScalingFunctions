#' Gamma variance from shape and rate parameter
#'
#' Calculation of the variance of a gamma distribution from its
#' shape and rate parameters
#' @param shape parameter of gamma distribution
#' @param rate parameter of gamma distribution
#' @return Variance of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_var_from_shape_rate <- function(shape, rate){
  var <- shape/rate^2
  names(var) <- "var"
  return(var)
}
