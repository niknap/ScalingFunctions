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



#' Rescale parameter
#'
#' Rescaling of distribution parameters (e.g., standard deviation) based on a
#' scaling exponent, i.e., a log-log-linear relationship (power law) between the area ratio of
#' input and output scales and the ratio of the known input parameter value and the unknown
#' output parameter value.
#' @param x Input value, e.g., known distribution parameter at a certain scale (reference scale)
#' @param x.scale Side length of a measurement unit of the input (reference scale)
#' @param y.scale Side length of a measurement unit of the output (target scale)
#' @param scaling.exp Scaling exponent
#' @return Parameter value at the target scale
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

rescale_par <- function(x, x.scale, y.scale, scaling.exp=-0.5){
  y <- (y.scale^2/x.scale^2)^scaling.exp*x
  return(y)
}

# Alternative name, same function
scaling_function <- function(x, x.scale, y.scale, scaling.exp=-0.5){
  y <- (y.scale^2/x.scale^2)^scaling.exp*x
  return(y)
}








