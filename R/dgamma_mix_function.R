# Function for PDF of mixed gamma distribution
dgamma_mix <- function(x, shape, rate){
  d.vec <- dgamma(x=x, shape=shape[1], rate=rate[1]) / length(shape)
  for(i in 2:length(shape)){
    d.vec <- d.vec + dgamma(x=x, shape=shape[i], rate=rate[i]) / length(shape)
  }
  return(d.vec)
}
