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



#' Variance of mixture distribution
#'
#' Calculation of the variance of a mixture distribution from means and variances of all contributing distributions
#' @param means Vector of means of all contributing distributions
#' @param vars Vector of variances of all contributing distributions
#' @param weights Vector of weights of all contributing distributions
#' @return Variance of mixture distribution
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

var_of_mixture <- function(means, vars, weights=NA){
  if(is.na(weights[1])){
    weights <- numeric(length(means))
    weights[] <- 1
  }
  weights <- weights/sum(weights)
  result <- sum(weights*(means^2+vars-mean_of_mixture(means, weights)^2))
  names(result) <- "var"
  return(result)
}


