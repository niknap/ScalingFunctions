#' Reconstruct probability density function from quantiles
#'
#' Reconstruct a PDF based on a set of quantiles of a distribution
#' or the quantile function (inverse cumulative distribution function ICDF)
#' @param q.vec Vector of quantiles (quantile function / inverse CDF; has to be
#' sorted in increasing order)
#' @param p.vec Vector of probabilities (values between 0 and 1 providing the
#' probability associated with each element of q.vec)
#' @param x.vec Vector of values for which the densities should be returned
#' @return Vector of probability densities for each element in x.vec
#' @keywords quantile function inverse CDF PDF density
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
#' @examples my.r.vec <- rnorm(n=50, mean=10, sd=6)
#' my.q.vec <- sort(my.r.vec)
#' my.p.vec <- pnorm(my.q.vec, mean=10, sd=6)
#' plot(my.q.vec ~ my.p.vec, type="l")
#'
#' my.x.vec <- -20:40
#' my.pdf.vec <- pdf_from_quantiles(q.vec=my.q.vec, p.vec=my.p.vec,
#' x.vec=my.x.vec)
#'
#' hist(my.r.vec, freq=F, xlim=c(-20, 40))
#' lines(my.pdf.vec ~ my.x.vec, col="red")

pdf_from_quantiles <- function(q.vec, p.vec, x.vec){
  len.q.vec <- length(q.vec)
  len.p.vec <- length(p.vec)
  len.x.vec <- length(x.vec)
  # q.vec and p.vec have to have the same length
  if(len.q.vec != len.p.vec){
    stop("Error: q.vec and p.vec have different lengths")
  }else
    # Check if the q.vec contains only one identical value (commonly zero)
    if(length(unique(q.vec[1:len.q.vec])) == 1){
      # If that is the case the density also only contains zeros
      pdf.vec <- rep(0, times=len.x.vec)
    }else{
      # Invert the quantile function to get to the CDF
      cdf.fun <- approxfun(x=q.vec, y=p.vec, rule=2)
      # Predict the cumulative probability for each x value based on CDF
      pred.cdf.vec <- cdf.fun(v=x.vec)
      # Calculate the probability density (PDF) by taking
      # the differences between all neighboring CDF values
      pdf.vec <- c(0, diff(pred.cdf.vec))
      # Standardize the PDF to a sum of 1
      pdf.vec <- round(pdf.vec/sum(pdf.vec), 5)
    }
  # Add x-values as names
  names(pdf.vec) <- x.vec
  return(pdf.vec)
}







