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



#' Arithmetic SD of lognormal distribution
#'
#' Calculation of the arithmetic standard deviation of a lognormal
#' distribution from function parameters
#' @param logmean Lognormal mean
#' @param logsd Lognormal standard deviation
#' @return Arithmetic standard deviation of lognormal distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

arithsd_of_lognorm <- function(logmean, logsd){
  arithsd <- ((exp(logsd^2)-1)*exp(2*logmean+logsd^2))^0.5
  names(arithsd) <- "sd"
  return(arithsd)
}
