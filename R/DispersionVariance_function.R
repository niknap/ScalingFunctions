# Function for calculating dispersion variance
# between two scales (this only works if the variogram
# model is fitted at the small scale)
# small.scale=100
# large.scale=10000
# vgm <- iso.vgm
DispersionVariance <- function(vgm, small.scale=100, large.scale=1000){
  require(gstat)
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
  vg.mx <- variogramLine(vgm, dist_vector=dist.mx, covariance=F)
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
  # vg.ras <- raster(vg.mx)
  # plot(vg.ras)
  dispersion.var <- sum(vg.mx*weight.mx, na.rm=T)/sum(weight.mx, na.rm=T)
  return(dispersion.var)
}
