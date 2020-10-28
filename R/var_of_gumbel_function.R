#' Variance of gumbel distribution
#'
#' Calculation of the variance of a gumbel
#' distribution from function parameter
#' @param scale Scale parameter of the gumbel distribution
#' @return Variance of gumbel distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

var_of_gumbel <- function(scale){
  var <- pi^2/6*scale^2
  names(var) <- "var"
  return(var)
}


