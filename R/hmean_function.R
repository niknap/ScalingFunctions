#' Harmonic mean
#'
#' Calculation of harmonic mean.
#' @param x Input vector
#' @param na.rm Boolean to remove missing values
#' @return Harmonic mean
#' @keywords harmonic mean average
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

hmean <- function(x, na.rm=T){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  x <- x[x != 0]
  n <- length(x)
  return(n / sum(1/x))
}
