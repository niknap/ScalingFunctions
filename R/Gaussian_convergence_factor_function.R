#' Scaling factor where distribution converges to Gaussian
#'
#' According to the central limit theorem distributions of aggregated variables
#' converge to a Gaussian shape with increasing aggregation. This function calculates
#' the aggregation factor between the current scale and the scale of convergence
#' for a given data vector. It is based on the Berry-Esseen
#' theorem, which provides an upper bound for the distance between standard normal
#' CDF and z-transformed empirical CDF (based on the 3rd moment). This distance between CDFs
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



#' is the Kolmogorov-Smirnov statistic D and for a desired significance level alpha the
#' critical value of D can be calculated.
#' @param vec Input data vector
#' @param alpha Significance level for the Kolmogorov-Smirnov statistic
#' @param BerryEsseen.const Berry-Esseen constant (there are different values found in the literature)
#' @return Scale factor between the current scale and the scale of Gaussian convergence (number of units that have to be aggregated, respectively)
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

Gaussian_convergence_factor <- function(vec, alpha=0.05, BerryEsseen.const=0.4748){
  require(moments)
  # Center and scale the data (z-transformation)
  scaled.vec <- scale(vec)
  # Calculate the 3rd moment
  rho <- moment(scaled.vec, 3)
  # Calculate the sample size at original scale
  N.orig <- length(scaled.vec)
  # Calculate numerator of the critical value for the Kolmogorov-Smirnov statistic
  num.crit <- (-0.5*log(alpha/2))^0.5
  # Use the Berry-Esseen theorem and enter the critical Kolmogorov-Smirnov statistic
  # to calculate the scaling factor of Gaussian convergence
  scaling.fac <- (BerryEsseen.const*rho*N.orig^0.5)/num.crit
  return(scaling.fac)
}


