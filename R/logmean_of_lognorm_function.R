#' Lognormal mean of lognormal distribution
#'
#' Calculation of the logarithmic mean parameter (mu)
#' of a lognormal distribution from distribution moments
#' @param arithmean Arithmetic mean
#' @param arithsd Arithmetic standard deviation
#' @return Logarithmic standard deviation of lognormal distribution (sigma
#' parameter)
#' @keywords mean average standard deviation SD moments lognormal arithmetic
#' logarithmic
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

logmean_of_lognorm <- function(arithmean, arithsd){
  logmean <- log(arithmean / (1 + arithsd^2/arithmean^2)^0.5)
  names(logmean) <- "logmean"
  return(logmean)
}
