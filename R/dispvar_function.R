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



#' Dispersion variance
#'
#' Calculation of dispersion variance between two scales. This only works if a
#' variogram model is available for the small scale. The dispersion variance
#' quantifies how much of the total variance of a variable at small scale is
#' happening inside the units of a larger units. You can also see it as the
#' proportion of variance being lost when upscaling from the small to the
#' large scale. The variance at large scale is the variance at small scale
#' minus the dispersion variance between the two scales.
#' @param vgm Variogram model (gstat) of a variable at small scale support
#' @param small.scale Side length of an area unit of the smaller scale
#' @param large.scale Side length of an area unit of the larger scale
#' @return Dispersion variance
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

dispvar <- function(vgm, small.scale=100, large.scale=1000){
  #require(gstat)
  my.nrow <- ceiling(large.scale/small.scale)+1
  my.ncol <- my.nrow
  my.center <- ceiling(large.scale/small.scale)/2+1
  # Generate a distance matrix
  dist.mx <- matrix(nrow=my.nrow, ncol=my.ncol)
  for(x in 1:my.nrow){
    for(y in 1:my.ncol){
      # Calculate the Euclidian distance from each cell to the center cell
      dist.mx[x, y] <- ((x-my.center)^2+(y-my.center)^2)^0.5*small.scale
    }
  }
  # Based on the distance matrix and the variogram model, generate a matrix
  # containing the variogram values
  vg.mx <- gstat::variogramLine(vgm, dist_vector=dist.mx, covariance=F)
  # Generate a weighting matrix, because if the large scale is an even
  # numbered multiple of the small scale the cells along the edges should
  # have only weight of 0.5 and in the corners 0.25
  weight.mx <- matrix(1, nrow=my.nrow, ncol=my.ncol)
  if(((large.scale/small.scale) %% 2) == 0){
    # Modify the edges
    weight.mx[1,] <- 0.5
    weight.mx[my.nrow,] <- 0.5
    weight.mx[, 1] <- 0.5
    weight.mx[, my.ncol] <- 0.5
    # Modify the corners
    weight.mx[1, 1] <- 0.25
    weight.mx[my.nrow, 1] <- 0.25
    weight.mx[1, my.ncol] <- 0.25
    weight.mx[my.nrow, my.ncol] <- 0.25
  }
  # Calculate dispersion variance as the (weighted) mean of all
  # values in the variance matrix
  dispersion.var <- sum(vg.mx*weight.mx, na.rm=T)/sum(weight.mx, na.rm=T)
  return(dispersion.var)
}
