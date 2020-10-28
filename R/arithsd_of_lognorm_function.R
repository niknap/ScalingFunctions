#' Arithmetic SD of lognormal distribution
#'
#' Calculation of the arithmetic standard deviation of a lognormal
#' distribution from function parameters
#' @param logmean Lognormal mean
#' @param logsd Lognormal standard deviation
#' @return Arithmetic standard deviation of lognormal distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

arithsd_of_lognorm <- function(logmean, logsd){
  arithsd <- ((exp(logsd^2)-1)*exp(2*logmean+logsd^2))^0.5
  names(arithsd) <- "sd"
  return(arithsd)
}
