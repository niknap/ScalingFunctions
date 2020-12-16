#' Arithmetic CV of lognormal distribution
#'
#' Calculation of the arithmetic coefficient of variation of a lognormal
#' distribution from function parameters
#' @param logsd Lognormal standard deviation
#' @return Arithmetic coefficient of variation of lognormal distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

arithcv_of_lognorm <- function(logsd){
  arithcv <- ((exp(logsd^2)-1))^0.5
  names(arithcv) <- "cv"
  return(arithcv)
}

