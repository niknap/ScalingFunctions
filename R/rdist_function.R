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



#' Random numbers from distribution
#'
#' Random number generation from arbitrary distribution defined by
#' a value vector x and a probability or frequency vector y. The function
#' uses inverse transform sampling.
#' @param n Number of samples that should be drawn from the distribution
#' @param x.vec Vector defining the values of the distribution
#' @param y.vec Vector defining the probability density or frequencies of the x values
#' @return Vector of random numbers
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

rdist <- function(n, x.vec, y.vec){
  # Normalize y to a total sum of 1 to obtain empirical PDF
  empir.pdf.vec <- y.vec / sum(y.vec)
  # Derive the empirical CDF
  empir.cdf.vec <- cumsum(empir.pdf.vec)
  # Invert the CDF based on linear interpolation of the given values
  # using approxfun() which returns a function.
  # Rule 2 assigns the extreme values if they fall out of the x range
  # (instead of NA)
  inv.cdf.fun <- approxfun(x=empir.cdf.vec, y=x.vec, rule=2)
  # Generate n uniformly distributed random numbers in the interval between 0 and 1
  runif.vec <- runif(n=n)
  # Calculate the corresponding random output values using the inverse CDF
  result.vec <- inv.cdf.fun(v=runif.vec)
  return(result.vec)
}
