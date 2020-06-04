#' Transform a probability density function with a function
#'
#' The PDF of an independent variable x is transformed into the PDF of
#' a dependent variable y, based on their relationship y=f(x).
#' @param x.vec Vector of values of the independent variable
#' @param x.d.vec Vector of probability densities of each x value
#' @param xy.func Function which relates x to y; Default is y=x, i.e. as an R
#' function function(x){1*x}
#' @param n.steps Number of discrete steps between which the PDF is
#' approximated via linear interpolation during the transformation
#' @return List containing 1) y values for each input x value, 2) x values for
#' all n steps, 3) y values for all n steps (of x) and 4) PDF values for all n
#' steps, as well as the functions for the PDFs of 5) x and 6) y as approxfun
#' functions
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
#' @examples # Simulate data from a lognormal distribution
#' my.x.vec <- rlnorm(200, 3.2, 0.3)
#' my.x.d.vec <- dlnorm(my.x.vec, 3.2, 0.3)
#' hist(my.x.vec, breaks=30, freq=F)
#' curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
#' # Define a power law relationship y=0.1*x^1.8
#' my.xy.func <- function(x){0.1*x^1.8}
#' # Apply the transformation
#' transform.list <- transform_pdf_with_function(x.vec=my.x.vec,
#' x.d.vec=my.x.d.vec, xy.func=my.xy.func, n.steps=1000)
#' hist(transform.list$y)
#' plot(transform.list$y ~ my.x.vec)
#' plot(transform.list$pdf.steps ~ transform.list$x.steps)
#' plot(transform.list$pdf.steps ~ transform.list$y.steps)

transform_pdf_with_function <- function(x.vec, x.d.vec,
                                        xy.func=function(x){1*x},
                                        n.steps=1000){
  # Calculate a y value for each x value using the given x-y-function
  y.vec <- xy.func(x.vec)
  # Sort by increasing x values
  order.vec <- order(x.vec)
  x.vec <- x.vec[order.vec]
  x.d.vec <- x.d.vec[order.vec]
  # Derive a function that approximates the PDF for x with linear
  # interpolation between given x density values
  x.pdf.fun <- approxfun(x=x.vec, y=x.d.vec, rule=2)
  # Generate n new x values with equal distances, called x.steps
  x.steps.vec <- seq(from=min(x.vec), to=max(x.vec), length.out=n.steps)
  # Calculate the probability density of each x.steps value
  d.vec <- x.pdf.fun(x.steps.vec)
  # Calculate a y value for each x.steps value using the given x-y-function
  y.steps.vec <- xy.func(x.steps.vec)
  # Derive a function that approximates the PDF for y with linear
  # interpolation between given density values
  y.pdf.fun <- approxfun(x=y.steps.vec, y=d.vec, rule=2)
  # Make a list of results for returning
  result.list <- list()
  result.list$y <- y.vec
  result.list$x.steps <- x.steps.vec
  result.list$y.steps <- y.steps.vec
  result.list$pdf.steps <- d.vec
  result.list$x.pdf.fun <- x.pdf.fun
  result.list$y.pdf.fun <- y.pdf.fun
  return(result.list)
}
