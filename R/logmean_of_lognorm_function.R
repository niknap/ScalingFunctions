# Calculate logarithmic SD of log-normal distribution from arithmetic stats
logsd_of_lognorm <- function(amean, asd){
  logsd <- (log(1 + arithsd^2/arithmean^2))^0.5
  return(logsd)
}
