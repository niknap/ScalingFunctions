#' Density of mixture distribution of aboveground biomass from white shot noise
#'
#' Probability density function for a mixture distribution of several gamma distributions
#' of aboveground biomass (AGB), which arise from biomass growth G and biomass mortality m
#' with G following a distribution and m being assumed as white shot noise. Under these
#' assumptions the distribution of AGB can be modelled as follows...
#' @param x Input vector of AGB values
#' @param G.dist Type of distribution of growth G ("lnorm" or "gamma")
#' @param G.par1 Parameter 1 of the distribution of G (logmean or gamma shape)
#' @param G.par2 Parameter 2 of the distribution of G (logsd or gamma rate)
#' @param m.alpha Mortality parameter 1: white shot noise mean intensity
#' @param m.lambda Mortality parameter 2: white shot noise mean frequency
#' @return Vector of densities for each value in x
#' @keywords mixture distribution gamma PDF density sum AGB growth gain mortality white shot noise
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

dmixture_AGB_from_white_shot_noise <- function(x, G.dist="lnorm", G.par1, G.par2, m.alpha, m.lambda){
  # Define a range of plausible G values
  G.vec <- seq(0.1, 100, 0.1)
  # Calculate the probability density for each of these G values
  if(G.dist == "lnorm"){
    d.G.vec <- dlnorm(G.vec, meanlog=G.par1, sdlog=G.par2)
  }else if(G.dist == "gamma"){
    d.G.vec <- dgamma(G.vec, shape=G.par1, rate=G.par2)
  }
  # Calculate the shape parameter of the gamma distribution of AGB
  # according to white shot noise theory
  gamma.shape <- 1/m.alpha+1
  # Calculate the rate parameters of the gamma distribution of AGB
  # according to white shot noise theory for each value of G
  gamma.rate.vec <- m.lambda/G.vec
  # Create a matrix with each column being one x value (AGB input)
  # and each row representing one possible G value and fill it with
  # the x values
  x.mx <- matrix(x, nrow=length(G.vec), ncol=length(x), byrow=T)
  # Calculate the probability density for each x value
  d.mx <- dgamma(x.mx, shape=gamma.shape, rate=gamma.rate.vec[])
  # Weight each density with the probality density of the
  # underlying G value
  w.vec <- d.G.vec / sum(d.G.vec)
  d.mx <- d.mx * w.vec
  # Add all PDFs to one mixture PDF
  d.vec <- as.vector(colSums(d.mx))
  return(d.vec)
}
