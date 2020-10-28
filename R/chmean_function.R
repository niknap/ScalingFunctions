#' Contraharmonic mean
#'
#' Calculation of contraharmonic mean.
#' @param x Input vector
#' @param na.rm Boolean to remove missing values
#' @return Contraharmonic mean
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

chmean <- function(x, na.rm=T){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  x <- x[x != 0]
  return(sum(x^2) / sum(x))
}
