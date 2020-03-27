#' Scale of gumbel distribution
#'
#' Calculation of the scale parameter (sometimes called beta) of a gumbel
#' distribution from its variance
#' @param var Variance of the gumbel distribution
#' @return Scale of gumbel distribution
#' @keywords variance standard deviation SD moments location scale gumbel arithmetic extreme value
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

scale_of_gumbel <- function(var){
  scale <- (6/pi^2*var)^0.5
  names(scale) <- "scale"
  return(scale)
}


