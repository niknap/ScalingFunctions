# Calculate median of log-normal distribution from function parameters
median_of_lognorm <- function(logmean){
  median <- exp(logmean)
  return(median)
}
