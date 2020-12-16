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



#' Gamma standard deviation from shape and rate parameter
#'
#' Calculation of the standard deviation (SD) of a gamma distribution from its
#' shape and rate parameters
#' @param shape parameter of gamma distribution
#' @param rate parameter of gamma distribution
#' @return SD of gamma distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gamma_sd_from_shape_rate <- function(shape, rate){
  sd <- shape^0.5/rate
  names(sd) <- "sd"
  return(sd)
}
