# Function for scaling of SD of lognormal distributions
lognorm_scaling <- function(logmean, logsd, scaling.fac, scaling.exp){
  # Calculate the arithmetic moments from the log moments
  my.amean <- calc.amean.lnorm(logmean=logmean, logsd=logsd)
  my.asd <- calc.asd.lnorm(logmean=logmean, logsd=logsd)
  # Rescale the SD
  scaled.asd <- my.asd * scaling.fac^scaling.exp
  # Calculate the log moments from the rescaled arithmetic moments
  my.logmean <- calc.logmean.lnorm(amean=my.amean, asd=scaled.asd)
  my.logsd <- calc.logsd.lnorm(amean=my.amean, asd=scaled.asd)
  # Return results
  results.vec <- c(my.logmean, my.logsd)
  names(results.vec) <- c("scaled.logmean", "scaled.logsd")
  return(results.vec)
}
