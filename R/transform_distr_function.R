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



#' Transform a distribution with a function
#'
#' The PDF of an independent variable x is transformed into the PDF of
#' a dependent variable y, based on their relationship y=f\(x\)
#' @param n Number of samples that should be drawn from the distribution
#' @param x.vec Vector defining the values of the distribution
#' @param y.vec Vector defining the probability density or frequencies of the x values
#' @return Vector of random numbers
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

# x.vec <- rlnorm(200, 3.2, 0.3)
# n.steps = 1000
# hist(x.vec, breaks=30, freq=F)
# xy.func <- function(x){0.1*x^1.8}
# xsdy.func <- function(x){1.2*x}
#
# xy.func=function(x){1*x}
# xsdy.func=function(x){1*x}




transform_distr <- function(x.vec, xy.func=function(x){1*x}, xsdy.func=function(x){1*x}, n.steps=1000){

  # Predict a y value for each x value
  y.vec <- xy.func(x.vec)
  # Predict a SD for each x value
  sdy.vec <- xsdy.func(x.vec)

  # Calculate all mean +/- 2.58 SDs to set the 99% predition
  # interval as limits
  upper.lim.vec <- y.vec + 2.58*sdy.vec
  lower.lim.vec <- y.vec - 2.58*sdy.vec

  # Define a vector of all y values for which to calculate the density
  y.steps.vec <- seq(from=min(lower.lim.vec), to=max(upper.lim.vec), length.out=n.steps)

  # Construct the PDF of y as a mixture of Gaussians
  # with all y values as means and the prediction SDs as SDs
  d.vec <- dnorm(x=y.steps.vec, mean=y.vec[1], sd=sdy.vec[1]) / length(y.vec)
  for(i in 2:length(y.vec)){
    d.vec <- d.vec + dnorm(x=y.steps.vec, mean=y.vec[i], sd=sdy.vec[i]) / length(y.vec)
  }

  # Normalize the density vector
  d.vec <- d.vec/sum(d.vec)

  # Derive the CDF
  cdf.vec <- cumsum(d.vec)
  cdf.fun <- approxfun(x=y.steps.vec, y=cdf.vec, rule=2)

  # Make a list of results for returning
  result.list <- list()
  result.list$y <- y.vec
  result.list$y.steps <- y.steps.vec
  result.list$y.dens <- d.vec
  result.list$cdf.fun <- cdf.fun

  return(result.list)
}





