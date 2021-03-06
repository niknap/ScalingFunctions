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



#' Harmonic mean
#'
#' Calculation of harmonic mean.
#' @param x Input vector
#' @param na.rm Boolean to remove missing values
#' @return Harmonic mean
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

hmean <- function(x, na.rm=T){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  x <- x[x != 0]
  n <- length(x)
  return(n / sum(1/x))
}
