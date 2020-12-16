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



# Functions for upscaling gamma distributions based on m and G
# (inputs alpha, lambda and G are vectors with values for different distributions;
# if lambda is left at the default value of 1, then alpha corresponds to m)

# Expected value
E_Bmean_from_m_G <- function(alpha, lambda=1, G){
  return(mean((1/alpha+1)*(G/lambda), na.rm=T))
}
