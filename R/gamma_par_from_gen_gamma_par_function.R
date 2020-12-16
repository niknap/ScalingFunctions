# Copyright (C) 2019 Dr. Nikolai Knapp, UFZ
#
# This file is part of the ScalingFunctions R package.
#
# The ScalingFunctions R package is free software: you can redistribute
# it and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# ScalingFunctions is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ScalingFunctions. If not, see <http://www.gnu.org/licenses/>.



#' Gamma parameters from generalized gamma parameters and transformation power
#' law coefficients
#'
#' If a variable x follows a gamma distribution the dependent variable y, which
#' is related to x via a power law relationship y=a*x^b follows a generalized
#' gamma distribution (GG). The parameters of the gamma distribution can be
#' calculated from the input GG parameters and the power law coefficients.
#' Two alternative formulations for the GG exist in the literature,
#' one of which uses parameters named a, d, p while the other one uses
#' mu, sigma, Q (e.g., R package flexsurv). Both parameter sets are
#' accepted by this function.
#' For parameter conversion see also:
#' https://en.wikipedia.org/wiki/Generalized_gamma_distribution
#' @param pl.fac Power law prefactor
#' @param pl.exp Power law exponent
#' @param gg.a GG parameter
#' @param gg.d GG parameter
#' @param gg.p GG parameter
#' @param gg.mu GG parameter (flexsurv form)
#' @param gg.sigma GG parameter (flexsurv form)
#' @param gg.Q GG parameter (flexsurv form)
#' @param flexsurv.form Switch whether to use the default GG parameter set
#' (a, d, p) or the flexsurv GG parameter set (mu, sigma, Q)
#' @return Vector containing gamma parameters shape, scale, rate as well as
#' descriptive statistics mean, var, sd and skewness of the gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_par_from_gen_gamma_par <- function(pl.fac, pl.exp, gg.a, gg.d, gg.p,
                                         gg.mu, gg.sigma, gg.Q,
                                         flexsurv.form=F){
  if(flexsurv.form == T){
    # Convert parameters mu, sigma and Q as in formulation used by the R
    # implementation in the flexsurv package to the standard GG parameters
    # Source: https://en.wikipedia.org/wiki/Generalized_gamma_distribution
    gg.a <- exp(gg.mu-(log(1/(gg.Q*gg.sigma))-log(gg.Q/gg.sigma))/
                  (gg.Q/gg.sigma))
    gg.d <- 1/(gg.Q*gg.sigma)
    gg.p <- gg.Q/gg.sigma
  }
  # Calculate gamma parameters from a, d, p and power law coefficients
  # Conversion: https://en.wikipedia.org/wiki/Gamma_distribution
  # Scaling with the prefactor is described here:
  # Source: https://math.stackexchange.com/questions/3143545/how-to-scale-a-generalized-gamma-distribution
  g.shape <- gg.d*pl.exp
  g.scale <- gg.a^(1/pl.exp)/pl.fac
  g.rate <- 1/g.scale
  # Moments of the gamma distribution
  g.mean <- g.shape/g.rate
  g.var <- g.shape/g.rate^2
  g.sd <- g.var^0.5
  g.skew <- 2/g.shape^0.5
  # Collect and return results
  res.vec <- c(g.shape, g.scale, g.rate, g.mean, g.var, g.sd, g.skew)
  names(res.vec) <- c("g.shape", "g.scale", "g.rate", "g.mean", "g.var",
                      "g.sd", "g.skew")
  return(res.vec)
}
