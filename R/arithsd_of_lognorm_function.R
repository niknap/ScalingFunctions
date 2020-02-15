# Calculate arithmetic SD of log-normal distribution from function parameters
arithsd_of_lnorm <- function(logmean, logsd){
  arithsd <- ((exp(logsd^2)-1)*exp(2*logmean+logsd^2))^0.5
  return(arithsd)
}
