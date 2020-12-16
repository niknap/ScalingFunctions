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




# Calculate power mean
pmean <- function(x, p=1, na.rm=T, zero.rm=F){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  if(zero.rm == T){
    x <- x[x != 0]
  }
  n <- length(x)
  # If p is 0 return the geometric mean,
  # which can be proven to be the limit
  # of the power mean in that case.
  # If p is + or - infinite the result
  # is the minimum or maximum of the
  # input vector.
  if(p == 0){
    return(prod(x^(1/n)))
  }else if(p == -Inf){
    return(min(x))
  }else if(p == Inf){
    return(max(x))
  }else{
    return((sum(x^p)/n)^(1/p))
  }
}
