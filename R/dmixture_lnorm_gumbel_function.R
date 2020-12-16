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
