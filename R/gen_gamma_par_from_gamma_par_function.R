#' Generalized gamma parameters from gamma parameters and transformation power
#' law coefficients
#'
#' If a variable x follows a gamma distribution the dependent variable y, which
#' is related to x via a power law relationship y=a*x^b follows a generalized
#' gamma distribution (GG). The parameters of the GG can be calculated from the
#' input gamma parameters and the power law coefficients. Two alternative
#' formulations for the GG exist in the literature, one of which uses parameters
#' named a, d, p while the other one uses mu, sigma, Q (e.g., R package
#' flexsurv). Both parameter sets are calculated by this function.
#' For parameter conversion see also:
#' https://en.wikipedia.org/wiki/Generalized_gamma_distribution
#' @param pl.fac Power law prefactor
#' @param pl.exp Power law exponent
#' @param g.shape Gamma shape parameter
#' @param g.rate Gamma rate parameter
#' @return Vector containing GG parameters a, d, p and mu, sigma, Q as well as
#' descriptive statistics mean, var and sd of the GG
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gen_gamma_par_from_gamma_par <- function(pl.fac, pl.exp, g.shape, g.rate){
  g.scale <- 1/g.rate
  # Parameters a, d and p as in standard formulation of the GG distributio
  # Relationship between gamma and GG parameters and power law coefficients
  # can be found here: https://en.wikipedia.org/wiki/Gamma_distribution
  gg.a <- g.scale^pl.exp*pl.fac
  gg.d <- g.shape/pl.exp
  gg.p <- 1/pl.exp
  # Scaling with the prefactor is described here:
  # Source: https://math.stackexchange.com/questions/3143545/how-to-scale-a-generalized-gamma-distribution
  # Parameters mu, sigma and Q as in formulation used by the R implementation
  # in the flexsurv package
  # Source: https://en.wikipedia.org/wiki/Generalized_gamma_distribution
  gg.mu <- log(gg.a)+(log(gg.d)-log(gg.p))/gg.p
  gg.sigma <- 1/(gg.p*gg.d)^0.5
  gg.Q <- (gg.p/gg.d)^0.5
  # Moments of the GG distribution
  gg.mean <- gg.a*((gamma((gg.d+1)/gg.p))/(gamma(gg.d/gg.p)))
  gg.var <- gg.a^2*((gamma((gg.d+2)/gg.p))/(gamma(gg.d/gg.p))-
                      ((gamma((gg.d+1)/gg.p))/(gamma(gg.d/gg.p)))^2)
  gg.sd <- (gg.var)^0.5
  # Collect and return results
  res.vec <- c(gg.a, gg.d, gg.p, gg.mu, gg.sigma, gg.Q, gg.mean, gg.var, gg.sd)
  names(res.vec) <- c("gg.a", "gg.d", "gg.p", "gg.mu", "gg.sigma", "gg.Q",
                      "gg.mean", "gg.var", "gg.sd")
  return(res.vec)
}
