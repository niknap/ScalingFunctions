# Calculate arithmetic CV of log-normal distribution from function parameters
arithcv_of_lognorm <- function(logsd){
  arithcv <- ((exp(logsd^2)-1))^0.5
  return(arithcv)
}

