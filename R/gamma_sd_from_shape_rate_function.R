# Gamma SD parameter from shape and rate
gamma_var_from_shape_rate <- function(shape, rate){
  sd <- shape^0.5/rate
  names(sd) <- "sd"
  return(sd)
}
