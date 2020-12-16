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



#' Density of mixed gamma distribution
#'
#' Probability density function for a mixture distribution of several gamma
#' distributions (all contributing with equal weights)
#' @param x Input vector of values
#' @param shape Vector of shape parameters of all contributing gamma
#' distributions
#' @param rate Vector of rate parameters of all contributing gamma
#' distributions
#' @return Vector of densities for each value in x
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

dgamma_mix <- function(x, shape, rate){
  d.vec <- dgamma(x=x, shape=shape[1], rate=rate[1]) / length(shape)
  for(i in 2:length(shape)){
    d.vec <- d.vec + dgamma(x=x, shape=shape[i], rate=rate[i]) / length(shape)
  }
  return(d.vec)
}
