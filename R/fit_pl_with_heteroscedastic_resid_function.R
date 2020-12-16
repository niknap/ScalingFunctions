#' Fit power law with heteroscedastic residuals
#'
#' Maximum likelihood fitting of power law relationship with heteroscedastic
#' residuals, which are fitted as well.
#'
#' The dependent variable y is assumed to be described as a power
#' law function of the independent variable:
#'
#' pred.y = a*x^b
#'
#' The residuals are assumed to follow a normal distribution around
#' the predicted y value and the hetercedastic standard deviation is
#' modelled as either a power law (pl)...
#'
#' sd.y = c*pred.y^d
#'
#' ... or a linear model (lm)...
#'
#' sd.y = c*pred.y+d
#'
#' ... of predicted y.
#'
#' Hence, the full model has four parameters and looks the following.
#'
#' pl case:
#'
#' y = a*x^b*Norm(mean=1, sd=c*(a*x^b)^d)
#'
#' lm case:
#'
#' y = a*x^b*Norm(mean=1, sd=c*(a*x^b)+d)
#'
#' @param x Vector of independent variable
#' @param y Vector of dependent variable
#' @param start Starting values for all four parameters a, b, c and d for the
#' optimization
#' @param sd.fun Type of function for the SD of residuals: either "pl" for power
#' law or "lm" for linear model
#' @return Fitted model object with four parameters
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
#' @export
#' @examples # Simulate data
#' x.vec <- seq(0.01, 1, 0.01)
#' # Without uncertainty
#' y.vec <- 10*x.vec^1.8
#' plot(y.vec ~ x.vec)
#' # With uncertainty
#' y.vec <- y.vec + rnorm(n=length(x.vec), mean=0, sd=3*x.vec)
#' plot(y.vec ~ x.vec)
#' # Fit with SD of residuals being a power law function of predicted y
#' fit.pl.resid <- fit_pl_with_heteroscedastic_resid(x=x.vec, y=y.vec, sd.fun="pl")
#' # Fit with SD of residuals being a linear function of predicted y
#' fit.lm.resid <- fit_pl_with_heteroscedastic_resid(x=x.vec, y=y.vec, sd.fun="lm")
#' # Draw the power law curve with with +- one SD (power law)
#' curve(fit.pl.resid$par[1]*x^fit.pl.resid$par[2], add=T, col="blue", lwd=3)
#' curve(fit.pl.resid$par[1]*x^fit.pl.resid$par[2]+(1*fit.pl.resid$par[3]*(fit.pl.resid$par[1]*x^fit.pl.resid$par[2])^fit.pl.resid$par[4]), add=T, col="blue", lwd=1, lty=2)
#' curve(fit.pl.resid$par[1]*x^fit.pl.resid$par[2]-(1*fit.pl.resid$par[3]*(fit.pl.resid$par[1]*x^fit.pl.resid$par[2])^fit.pl.resid$par[4]), add=T, col="blue", lwd=1, lty=2)
#' # Draw the power law curve with with +- one SD (power law)
#' curve(fit.lm.resid$par[1]*x^fit.lm.resid$par[2], add=T, col="red", lwd=3)
#' curve(fit.lm.resid$par[1]*x^fit.lm.resid$par[2]+(1*fit.lm.resid$par[1]*fit.lm.resid$par[3]*x^fit.lm.resid$par[2]+fit.lm.resid$par[4]), add=T, col="red", lwd=1, lty=2)
#' curve(fit.lm.resid$par[1]*x^fit.lm.resid$par[2]-(1*fit.lm.resid$par[1]*fit.lm.resid$par[3]*x^fit.lm.resid$par[2]+fit.lm.resid$par[4]), add=T, col="red", lwd=1, lty=2)

fit_pl_with_heteroscedastic_resid <- function(x, y, start=c(1, 1, 1, 1),
                                              sd.fun="pl"){
  # Define the negative log-likelihood function
  negloglik_heteroscedastic_norm <- function(para, xdat, ydat)
  {
    # Power law model for prediction of y from x
    my.ypred <- para[1]*xdat^para[2]
    # Calculate log-likelihood sum for observed y data under the proposed
    # prediction model with residuals following a normal distribution with
    # mean being the proposed predicted y value and SD being a function of
    # the predicted y value, which...
    if (sd.fun=="pl") {
      # ... follows a power law equation
      LL <- sum(dnorm(ydat, mean=my.ypred, sd=para[3]*my.ypred^para[4], log=T))
    }else if (sd.fun=="lm") {
      # ... follows a linear equation
      LL <- sum(dnorm(ydat, mean=my.ypred, sd=para[3]*my.ypred+para[4], log=T))
    }
    # Return the negative log-likelihood for minimization
    return(-LL)
  }
  # Minimize the negative log-likelihood to fit the model to the data
  suppressWarnings(fit <- optim(par=start, fn=negloglik_heteroscedastic_norm,
                                xdat=x, ydat=y, method="BFGS",
                                control=list(maxit=100)))
  return(fit)
}
