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



#' Lognormal mean of lognormal distribution
#'
#' Calculation of the logarithmic mean parameter (mu)
#' of a lognormal distribution from distribution moments
#' @param arithmean Arithmetic mean
#' @param arithsd Arithmetic standard deviation
#' @return Logarithmic standard deviation of lognormal distribution (sigma
#' parameter)
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

logmean_of_lognorm <- function(arithmean, arithsd){
  logmean <- log(arithmean / (1 + arithsd^2/arithmean^2)^0.5)
  names(logmean) <- "logmean"
  return(logmean)
}
