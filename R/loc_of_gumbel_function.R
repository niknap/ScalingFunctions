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



#' Location parameter of gumbel distribution
#'
#' Calculation of the location parameter (sometimes called alpha) of a gumbel
#' distribution from its mean and either variance or scale parameter (don't
#' provide both)
#' @param mean Mean of the gumbel distribution
#' @param var Variance of the gumbel distribution
#' @param scale Scale parameter of the gumbel distribution
#' @return Location of gumbel distribution

#' @author Nikolai Knapp, nikolai.knapp@ufz.de

loc_of_gumbel <- function(mean, var=NA, scale=NA){
  # Negative digamma(1) is the Euler-Mascheroni constant
  if(!is.na(var) & is.na(scale)){
    loc <- mean - scale_of_gumbel(var)*(-digamma(1))
  }else if(is.na(var) & !is.na(scale)){
    loc <- mean - scale*(-digamma(1))
  }else{
    loc <- NA
    warning("Provide either var or scale, but not both")
  }
  names(loc) <- "loc"
  return(loc)
}
