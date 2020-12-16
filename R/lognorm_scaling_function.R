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



# Function for scaling of SD of lognormal distributions
lognorm_scaling <- function(logmean, logsd, scaling.fac, scaling.exp){
  # Calculate the arithmetic moments from the log moments
  my.amean <- calc.amean.lnorm(logmean=logmean, logsd=logsd)
  my.asd <- calc.asd.lnorm(logmean=logmean, logsd=logsd)
  # Rescale the SD
  scaled.asd <- my.asd * scaling.fac^scaling.exp
  # Calculate the log moments from the rescaled arithmetic moments
  my.logmean <- calc.logmean.lnorm(amean=my.amean, asd=scaled.asd)
  my.logsd <- calc.logsd.lnorm(amean=my.amean, asd=scaled.asd)
  # Return results
  results.vec <- c(my.logmean, my.logsd)
  names(results.vec) <- c("scaled.logmean", "scaled.logsd")
  return(results.vec)
}
