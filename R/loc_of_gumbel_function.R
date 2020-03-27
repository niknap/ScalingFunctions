#' Location parameter of gumbel distribution
#'
#' Calculation of the location parameter (sometimes called alpha) of a gumbel
#' distribution from its mean and either variance or scale parameter (don't provide both)
#' @param mean Mean of the gumbel distribution
#' @param var Variance of the gumbel distribution
#' @param scale Scale parameter of the gumbel distribution
#' @return Location of gumbel distribution
#' @keywords variance mean moments location scale gumbel arithmetic extreme value
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

loc_of_gumbel <- function(mean, var=NA, scale=NA){
  # Negative digamma(1) is the Euler-Mascheroni constant
  if(!is.na(var) & is.na(scale)){
    loc <- mean - scale_of_gumbel(var)*(-digamma(1))
  }else if(is.na(var) & !is.na(scale)){
    loc <- mean - scale*(-digamma(1))
  }else{
    loc <- NA
    warning("Provide either var or scale, but not both")
  }
  names(loc) <- "loc"
  return(loc)
}
