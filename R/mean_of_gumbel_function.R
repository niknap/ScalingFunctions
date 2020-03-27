#' Mean of gumbel distribution
#'
#' Calculation of the arithmetic mean of a gumbel
#' distribution from function parameters
#' @param loc Location parameter of the gumbel distribution
#' @param scale Scale parameter of the gumbel distribution
#' @return Arithmetic mean of gumbel distribution
#' @keywords mean average moments location scale gumbel arithmetic extreme value
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

mean_of_gumbel <- function(loc, scale){
  # Negative digamma(1) is the Euler-Mascheroni constant
  mean <- loc+scale*(-digamma(1))
  names(mean) <- "mean"
  return(mean)
}
