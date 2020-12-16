#' Transform a probability density function with a function
#'
#' The PDF of an independent variable x is transformed into the PDF of
#' a dependent variable y based on their relationship y=f(x). Optionally,
#' uncertainty in the x-y-relationship can be incorporated by providing
#' a function that describes the SD of y (assuming Gaussian uncertainty
#' in y direction) either as a function of x or as a function of predicted y.
#' @param x.vec Vector of values of the independent variable
#' @param x.d.vec Vector of probability densities of each x value
#' @param xy.func Function which relates x to y; Default is y=x
#' @param xsdy.func Function which relates x to SD of y; Default is NA,
#' i.e., no uncertainty
#' @param ysdy.func Function which relates y (i.e., f(x)) to SD of y;
#' Default is NA, i.e., no uncertainty
#' @param n.steps Number of discrete steps between which the PDF is
#' approximated via linear interpolation during the transformation
#' @return List containing 1) y values for each input x value, 2) x values for
#' all n steps, 3) y values for all n steps (of x; in the case with uncertainty
#' included the range of y steps is wider than the range of f(x); it ranges
#' from min(f(x)) minus the 99% prediction interval to max(f(x)) plus the 99%
#' prediction interval of y), 4) PDF values for all x steps, 5) PDF values for
#' all y steps, as well as the functions for the PDFs of 6) x and 7) y as
#' approxfun functions
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
#' @examples # Simulate data from a lognormal distribution
#' my.x.vec <- rlnorm(200, 3.2, 0.3)
#' my.x.d.vec <- dlnorm(my.x.vec, 3.2, 0.3)
#' hist(my.x.vec, breaks=30, freq=F)
#' curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
#' points(my.x.d.vec ~ my.x.vec, col="red")
#' # Define a power law relationship y=0.1*x^1.8
#' my.xy.func <- function(x){0.1*x^1.8}
#' # Apply the transformation
#' transform.list <- transform_pdf_with_function(x.vec=my.x.vec,
#' x.d.vec=my.x.d.vec, xy.func=my.xy.func, n.steps=1000)
#' plot(transform.list$y ~ my.x.vec)
#' plot(transform.list$x.steps.pdf ~ transform.list$x.steps)
#' plot(transform.list$y.steps.pdf ~ transform.list$y.steps)
#' hist(transform.list$y, breaks=30, freq=F)
#' new.y.steps.vec <- 1:150
#' new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
#' lines(new.y.pdf.vec ~ new.y.steps.vec, col="blue")

transform_pdf_with_function <- function(x.vec, x.d.vec,
                                        xy.func=function(x){1*x},
                                        xsdy.func=NA,
                                        ysdy.func=NA,
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
  x.steps.pdf.vec <- x.pdf.fun(x.steps.vec)
  # Calculate a y value for each x.steps value using the given x-y-function
  y.steps.vec <- xy.func(x.steps.vec)
  # Distinguish between the cases 1) exact x-y-relationship and 2) uncertainty
  # given as SD of y either given as a) a function of x or as b) a function of y
  if (is.na(xsdy.func) & is.na(ysdy.func)) {
    # Derive a function that approximates the PDF for y with linear
    # interpolation between given density values. As density for each y value,
    # the density of the corresponding x value is used
    y.pdf.fun <- approxfun(x=y.steps.vec, y=x.steps.pdf.vec, rule=2)
    # Calculate the integral under the PDF of y to correct the density
    # values and refit the approxfun to ensure a total probability (integral)
    # of 1
    y.pdf.integral <- integrate(f=y.pdf.fun, lower=min(y.vec), upper=max(y.vec))
    y.steps.pdf.vec <- x.steps.pdf.vec / y.pdf.integral[[1]]
    y.pdf.fun <- approxfun(x=y.steps.vec, y=y.steps.pdf.vec, rule=2)
  } else {
    if (!is.na(xsdy.func) & is.na(ysdy.func)) {
      # Predict an SD of y for each x.steps value
      sdy.steps.vec <- xsdy.func(x.steps.vec)
    } else if (is.na(xsdy.func) & !is.na(ysdy.func)) {
      # Predict an SD of y for each y.steps value
      sdy.steps.vec <- ysdy.func(y.steps.vec)
    } else {
      stop("Don't provide an xsdy.func and ysdy.func simultaneously.
           Please set one of them to NA.")
    }
    # Calculate all means +/- 2.58 SDs to set the 99% predition
    # intervals as limits
    upper.lim.vec <- y.steps.vec + 2.58*sdy.steps.vec
    lower.lim.vec <- y.steps.vec - 2.58*sdy.steps.vec
    # Define a new y.steps vector, which streches from the minimum lower
    # prediction limit to the maximum upper prediction limit
    y.lim.steps.vec <- seq(from=min(lower.lim.vec), to=max(upper.lim.vec),
                           length.out=n.steps)
    # Construct the PDF of y as a mixture of Gaussians
    # with all y.steps values as means and the prediction SDs as SDs
    # and weighted with the PDF values of the corresponding x values
    y.steps.pdf.vec <- dnorm(x=y.lim.steps.vec, mean=y.steps.vec[1],
                             sd=sdy.steps.vec[1]) * x.steps.pdf.vec[1]
    for(i in 2:length(y.steps.vec)){
      y.steps.pdf.vec <- y.steps.pdf.vec + dnorm(x=y.lim.steps.vec,
                                                 mean=y.steps.vec[i],
                                                 sd=sdy.steps.vec[i]) *
        x.steps.pdf.vec[i]
    }
    # Derive a function that approximates the PDF for y with linear
    # interpolation between given density values.
    y.pdf.fun <- approxfun(x=y.lim.steps.vec, y=y.steps.pdf.vec, rule=2)
    # Calculate the integral under the PDF of y to correct the density
    # values and refit the approxfun to ensure a total probability (integral)
    # of 1
    y.pdf.integral <- integrate(f=y.pdf.fun, lower=min(y.lim.steps.vec),
                                upper=max(y.lim.steps.vec))
    y.steps.pdf.vec <- y.steps.pdf.vec / y.pdf.integral[[1]]
    y.pdf.fun <- approxfun(x=y.lim.steps.vec, y=y.steps.pdf.vec, rule=2)
    }
  # Make a list of results for returning
  result.list <- list()
  result.list$y <- y.vec
  result.list$x.steps <- x.steps.vec
  if(is.na(xsdy.func) & is.na(ysdy.func)){
    result.list$y.steps <- y.steps.vec
  } else {
    result.list$y.steps <- y.lim.steps.vec
  }
  result.list$x.steps.pdf <- x.steps.pdf.vec
  result.list$y.steps.pdf <- y.steps.pdf.vec
  result.list$x.pdf.fun <- x.pdf.fun
  result.list$y.pdf.fun <- y.pdf.fun
  return(result.list)
}
