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



#' Rank transformation based on a vector of target values
#'
#' Rank transform a dataset, i.e., for each input value provide an output value
#' from a given target vector. The target vector will be rearranged such that
#' the ranking of values corresponds the ranking of the input vector values.
#' @param x Input vector
#' @param y Target vector
#' @return Vector of transformed values
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

rank_transform_vec <- function(x, y){
  # Check for consistent length of x and y vectors
  if(length(x) == length(y)){
    # Sort y by size
    sorted.y <- sort(y)
    # Reorder y by the order of x
    reordered.y <- sorted.y[order(x)]
    return(reordered.y)
  }else{
    warning("Lengths of x and y are different")
  }
}


