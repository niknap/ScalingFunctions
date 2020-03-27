# Gamma variance parameter from shape and rate
gamma_var_from_shape_rate <- function(shape, rate){
  var <- shape/rate^2
  names(var) <- "var"
  return(var)
}
