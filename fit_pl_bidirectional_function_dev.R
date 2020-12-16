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

fit_pl_bidirectional <- function(x, y, start=c(1, 1, 1, 1)){
  # Define the negative log-likelihood function
  negloglik_norm <- function(para, xdat, ydat)
  {
    # Model for prediction
    my.ypred <- para[1]*xdat^para[2]
    # Heteroscedastic normal residual distribution
    LL <- sum(dnorm(ydat, mean=my.ypred, sd=1, log=T))
    # Use negative log-likelihood
    return(-LL)
  }
  # Minimize the negative log-likelihood to fit the model to the data with y over x
  suppressWarnings(fit.y.over.x <- optim(par=start[c(1, 2)], fn=negloglik_norm,
                                          xdat=x, ydat=y, method="BFGS",
                                          control=list(maxit=100)))
  # Minimize the negative log-likelihood to fit the model to the data with x over y
  suppressWarnings(fit.x.over.y <- optim(par=start[c(3, 4)], fn=negloglik_norm,
                                         xdat=y, ydat=x, method="BFGS",
                                         control=list(maxit=100)))
  # Collect the coefficients of the fits from both directions
  a.yx <- fit.y.over.x$par[1]
  b.yx <- fit.y.over.x$par[2]
  a.xy <- fit.x.over.y$par[1]
  b.xy <- fit.x.over.y$par[2]
  # Calculate the prefactor of a new power law which is the geometric mean
  # of two fitted power laws
  a <- (1/a.xy)^(1/(2*b.xy))*a.yx^(1/2)
  # Calculate the exponent of a new power law which is the geometric mean
  # of two fitted power laws
  b <- 1/2*(1/b.xy+b.yx)
  return(c(a, b))
  #return(c(a.yx, b.yx, a.xy, b.xy))
}



#########################
# Test it
#########################

require(slidaRtools)
require(data.table)

# Read the BCI data
wd <- "C:\\Users\\knappn\\Desktop\\ReducedComplexityProject\\ScalingWorkflow2\\"
setwd(wd)
attr.dt <- readRDS("ForestAttributes.dt4_use_DFstatus_CorrectNegG.rds")
stats.dt <- readRDS(paste0(wd, "RMarkdownScalingDocumentation\\ScriptsGraphics\\stats_FittingDistributions.dt.rds"))
scales.vec <- c(10, 20, 50, 100)

# Do it for TCH
chm.dt <- readRDS("bci.chm.dt.rds")
chm.ras <- raster.from.point.cloud(chm.dt)
plot(chm.ras)


# Prepare data at 20-m scale
chm.dt[, SpatID := calc.spatial.index(X, Y, res=20)]
tch.20.dt <- chm.dt[, .(TCH = mean(Z, na.rm=T)), keyby=SpatID]
tch.vec <- tch.20.dt$TCH

attr.20.dt <- subset(attr.dt, Scale==20 & Census==6)
nrow(attr.20.dt)
nrow(tch.20.dt)
head(attr.20.dt)
head(tch.20.dt)

setorderv(attr.20.dt, cols=c("SpatID.20"))
setorderv(tch.20.dt, cols=c("SpatID"))

attr.20.dt <- cbind(attr.20.dt, tch.20.dt)

par(mar=c(5,5,1,1))
plot(attr.20.dt$AGB.per.ha ~ attr.20.dt$TCH, xlab="TCH [m]", ylab="AGB [t/ha]")
plot(attr.20.dt$TCH ~ attr.20.dt$AGB.per.ha, xlab="AGB [t/ha]", ylab="TCH [m]")

b.vec <- attr.20.dt$AGB.per.ha
h.vec <- attr.20.dt$TCH


# Test
(test.fit <- fit_pl_bidirectional(x=h.vec, y=b.vec, start=c(0.5, 2, 2, 0.5)))

#(1/a.xy)^(1/(2*b.xy))*a.yx^(1/2)
(1/-89.6994544)^(1/(2*-584.0879472))*0.7991916^(1/2)


plot(b.vec ~ h.vec, xlab="TCH [m]", ylab="AGB [t/ha]")
curve(test.fit[1]*x^test.fit[2], add=T, col="gold", lwd=4, lty=1)


# Test in other direction
(test2.fit <- fit_pl_bidirectional(x=b.vec, y=h.vec, start=c(2, 0.5, 0.5, 2)))

test.agb.vec <- 1:2000
test.tch.vec <- test2.fit[1]*test.agb.vec^test2.fit[2]
lines(test.agb.vec~test.tch.vec, lty=2, col="blue")


#(1/a.xy)^(1/(2*b.xy))*a.yx^(1/2)
(1/-89.6994544)^(1/(2*-584.0879472))*0.7991916^(1/2)


plot(b.vec ~ h.vec, xlab="TCH [m]", ylab="AGB [t/ha]")
curve(test.fit[1]*x^test.fit[2], add=T, col="gold", lwd=4, lty=1)





log.b.vec <- log10(attr.20.dt$AGB.per.ha)
log.h.vec <- log10(attr.20.dt$TCH)

plot(log.b.vec ~ log.h.vec)
lm.fit <- lm(log.b.vec ~ log.h.vec)
curve(lm.fit$coef[2]*x+lm.fit$coef[1], col="blue", add=T)
#abline(lm.fit, col="gold")
#plot(log.b.vec ~ log.h.vec)
lm2.fit <- lm(log.h.vec ~ log.b.vec)
curve((1/lm2.fit$coef[2])*x-(lm2.fit$coef[1]/lm2.fit$coef[2]), col="red", add=T)

# y = a*x+b
# x = (y-b)/a
# x = 1/a*y - b/a
# y = (1/a)*x-b/a


# Fit 1:
# y = a1*x^b1
# log(y) = log(a1)+b1*log(x)

# Fit 2:
# x = a2*y^b2
# log(x) = log(a2)+b2*log(y)

# Inversion:
# log(y) = 1/b2*log(x) - log(a2)/b2

# Averaging:
# mean.prefactor = (b1 + 1/b2) / 2
# mean.intercept = (log(a1)-log(a2)/b2) / 2

log.a1 <- lm.fit$coef[1]
b1 <- lm.fit$coef[2]
log.a2 <- lm2.fit$coef[1]
b2 <- lm2.fit$coef[2]

mean.slope <- (b1 + 1/b2) / 2
mean.intercept <- (log.a1-log.a2/b2) / 2


# original scale
plot(b.vec ~ h.vec)
curve(10^(log.a1)*x^b1, col="blue", add=T)
curve(10^(-log.a2/b2)*x^(1/b2), col="red", add=T)
curve(10^mean.intercept*x^mean.slope, col="green", add=T)

pred1.b <- 10^(log.a1)*h.vec^b1
pred2.b <- 10^(-log.a2/b2)*h.vec^(1/b2)
predmean.b <- 10^mean.intercept*h.vec^mean.slope

plot(pred1.b ~ b.vec)
plot(pred2.b ~ b.vec)
plot(predmean.b ~ b.vec)

# RMSE
(mean((pred1.b - b.vec)^2))^0.5
(mean((pred2.b - b.vec)^2))^0.5
(mean((predmean.b - b.vec)^2))^0.5

# R2
summary(lm(pred1.b ~ b.vec))$r.squared
summary(lm(pred2.b ~ b.vec))$r.squared
summary(lm(predmean.b ~ b.vec))$r.squared

# Bias
mean(pred1.b - b.vec)
mean(pred2.b - b.vec)
mean(predmean.b - b.vec)














