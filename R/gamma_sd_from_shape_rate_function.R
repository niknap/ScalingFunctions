#' Gamma standard deviation from shape and rate parameter
#'
#' Calculation of the standard deviation (SD) of a gamma distribution from its
#' shape and rate parameters
#' @param shape parameter of gamma distribution
#' @param rate parameter of gamma distribution
#' @return SD of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_sd_from_shape_rate <- function(shape, rate){
  sd <- shape^0.5/rate
  names(sd) <- "sd"
  return(sd)
}
