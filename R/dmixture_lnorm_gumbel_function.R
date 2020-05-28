#' Density of mixture distribution of lognormal and gumbel distribution
#'
#' Probability density function for a mixture distribution composed of one
#' lognormal and one gumbel distribution. The gumbel distribution is suited to
#' model the extreme values in a heavy-tail dataset, while the lognormal models
#' the regular values.
#' @param x Input vector of values
#' @param logmean Logmean parameter of the lognormal distribution
#' @param logsd Logsd parameter of the lognormal distribution
#' @param gumbel.alpha Location parameter of the gumbel distribution
#' @param gumbel.scale Scale parameter of the gumbel distribution
#' @param w.gumbel Weight of the gumbel distribution within the mixture (weight
#' of lognormal is 1-w.gumbel)
#' @return Vector of densities for each value in x
#' @keywords mixture distribution lognormal gumbel PDF density sum extreme
#' value theory outlier
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

dmixture_lnorm_gumbel <- function(x, logmean, logsd, gumbel.alpha,
                                  gumbel.scale, w.gumbel=0.05){
  stopifnot(w.gumbel >= 0 & w.gumbel <= 1)
  w.lnorm <- 1-w.gumbel
  d.lnorm.vec <- dlnorm(x, meanlog=logmean, sdlog=logsd)
  d.gumbel.vec <- actuar::dgumbel(x, alpha=gumbel.alpha, scale=gumbel.scale)
  d.mixture.vec <- w.lnorm*d.lnorm.vec+w.gumbel*d.gumbel.vec
  return(d.mixture.vec)
}
