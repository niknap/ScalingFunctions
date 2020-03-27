# Gamma shape parameter from mean and variance
gamma_shape_from_mean_var <- function(mean, var){
  shape <- mean^2/var
  names(shape) <- "shape"
  return(shape)
}
