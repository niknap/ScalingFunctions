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



#' Rank transformation based on a distribution function
#'
#' Rank transform a dataset, i.e., for each input value provide an output value
#' from a given probability distribution, with the order of values (ranks)
#' being preserved.
#' @param x Input vector
#' @param distr Character naming output distribution function (e.g. "norm",
#' "lnorm", "unif", "exp", "gamma",...). Use "trunc" for truncated distributions
#' as provided by the truncdist package. See examples below for parameter
#' settings.
#' @param ... Distribution parameters (see examples below)
#' @return Vector of transformed values
#' @keywords rank transformation distribution order normalization
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
#' @examples # Transform values from 1 to 100 into a normal distribution
#' test.vec <- 1:100
#' norm.vec <- rank_transform_distr(x=test.vec, distr="norm", mean=55, sd=5)
#' hist(test.vec)
#' hist(norm.vec)
#' # Transform into a gamma distribution
#' gamma.vec <- rank_transform_distr(x=norm.vec, distr="gamma",
#'                                   shape=0.5, rate=1)
#' hist(gamma.vec)
#' # Transform into a truncated lognormal distribution
#' require(truncdist)
#' trunc.lnorm.vec <- rank_transform_distr(x=gamma.vec, distr="trunc",
#'                                         meanlog=3, sdlog=0.5,
#'                                         spec="lnorm", a=10, b=40)
#' hist(trunc.lnorm.vec)

rank_transform_distr <- function(x, distr="norm", ...){
  # Derive the empirical cumulative distribution function (ECDF) of the
  # input data
  input.ecdf <- ecdf(x)
  # For each element in x calculate which quantile of the ECDF it is
  quantiles.vec <- input.ecdf(x)
  # Get the inverse cumulative distribution function (ICDF), also called
  # quantile function, of the output data
  output.icdf <- get(paste0("q", distr), mode="function")
  # Calculate the output value from each input's quantile
  output.vec <- output.icdf(quantiles.vec, ...)
  return(output.vec)
}

