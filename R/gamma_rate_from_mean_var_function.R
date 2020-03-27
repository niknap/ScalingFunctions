# Gamma rate parameter from mean and variance
gamma_rate_from_mean_var <- function(mean, var){
  rate <- mean/var
  names(rate) <- "rate"
  return(rate)
}
