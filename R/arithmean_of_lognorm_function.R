#' Arithmetic mean of lognormal distribution
#'
#' Calculation of the arithmetic mean of a lognormal
#' distribution from function parameters
#' @param logmean Lognormal mean
#' @param logsd Lognormal standard deviation
#' @return Arithmetic mean of lognormal distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

arithmean_of_lognorm <- function(logmean, logsd){
  arithmean <- exp(logmean + logsd^2/2)
  names(arithmean) <- "mean"
  return(arithmean)
}
