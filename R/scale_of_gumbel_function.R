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



#' Scale of gumbel distribution
#'
#' Calculation of the scale parameter (sometimes called beta) of a gumbel
#' distribution from its variance
#' @param var Variance of the gumbel distribution
#' @return Scale of gumbel distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

scale_of_gumbel <- function(var){
  scale <- (6/pi^2*var)^0.5
  names(scale) <- "scale"
  return(scale)
}


