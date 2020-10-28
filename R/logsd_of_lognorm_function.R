#' Lognormal SD of lognormal distribution
#'
#' Calculation of the logarithmic standard deviation parameter (sigma)
#' of a lognormal distribution from distribution moments
#' @param arithmean Arithmetic mean
#' @param arithsd Arithmetic standard deviation
#' @return Logarithmic standard deviation of lognormal distribution (sigma parameter)
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

logsd_of_lognorm <- function(arithmean, arithsd){
  logsd <- (log(1 + arithsd^2/arithmean^2))^0.5
  names(logsd) <- "logsd"
  return(logsd)
}

