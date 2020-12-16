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



#' Gamma rate parameter from mean and variance
#'
#' Calculation of the rate parameter of a gamma distribution from its
#' mean and variance
#' @param mean of gamma distribution
#' @param variance of gamma distribution
#' @return Rate parameter of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_rate_from_mean_var <- function(mean, var){
  rate <- mean/var
  names(rate) <- "rate"
  return(rate)
}
