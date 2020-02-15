# Calculate arithmetic mean of log-normal distribution from function parameters
arithmean_of_lognorm <- function(logmean, logsd){
  arithmean <- exp(logmean + logsd^2/2)
  return(arithmean)
}
