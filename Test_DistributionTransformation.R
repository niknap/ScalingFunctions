
# x.vec <- rlnorm(200, 3.2, 0.3)
# n.steps = 1000
# hist(x.vec, breaks=30, freq=F)
# xy.func <- function(x){0.1*x^1.8}
# xsdy.func <- function(x){1.2*x}
#
# xy.func=function(x){1*x}
# xsdy.func=function(x){1*x}




transform_distr <- function(x.vec, xy.func=function(x){1*x}, xsdy.func=function(x){1*x}, n.steps=1000){

  # Predict a y value for each x value
  y.vec <- xy.func(x.vec)
  # Predict a SD for each x value
  sdy.vec <- xsdy.func(x.vec)

  # Calculate all mean +/- 2.58 SDs to set the 99% predition
  # interval as limits
  upper.lim.vec <- y.vec + 2.58*sdy.vec
  lower.lim.vec <- y.vec - 2.58*sdy.vec

  # Define a vector of all y values for which to calculate the density
  y.steps.vec <- seq(from=min(lower.lim.vec), to=max(upper.lim.vec), length.out=n.steps)

  # Construct the PDF of y as a mixture of Gaussians
  # with all y values as means and the prediction SDs as SDs
  d.vec <- dnorm(x=y.steps.vec, mean=y.vec[1], sd=sdy.vec[1]) / length(y.vec)
  for(i in 2:length(y.vec)){
    d.vec <- d.vec + dnorm(x=y.steps.vec, mean=y.vec[i], sd=sdy.vec[i]) / length(y.vec)
  }

  # Normalize the density vector
  #d.vec <- d.vec/sum(d.vec)

  # Derive the CDF
  cdf.vec <- cumsum(d.vec)
  cdf.fun <- approxfun(x=y.steps.vec, y=cdf.vec, rule=2)

  # Make a list of results for returning
  result.list <- list()
  result.list$y <- y.vec
  result.list$y.steps <- y.steps.vec
  result.list$y.dens <- d.vec
  result.list$cdf.fun <- cdf.fun

  return(result.list)
}



x.vec <- rlnorm(200, 3.2, 0.3)
n.steps = 1000
hist(x.vec, breaks=30, freq=F)
curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
xy.func <- function(x){0.1*x^1.8}
xsdy.func <- function(x){1.2*x}


trans.list <- transform_distr(x.vec=x.vec, xy.func=xy.func, xsdy.func=xsdy.func)
str(trans.list)

require(ScalingFunctions)
rand.y.val.vec <- rdist(n=1000, x.vec=trans.list$y.steps, y.vec=trans.list$y.dens)
hist(rand.y.val.vec, breaks=30, freq=F)
lines(trans.list$y.dens ~ trans.list$y.steps, col="red")






xy.func <- function(x){0.2*x^1.8}
xsdy.func <- function(x){1.2*x}

y.vec <- xy.func(x.vec)
sdy.vec <- xsdy.func(x.vec)
upper.lim.vec <- y.vec + 2.58*sdy.vec
lower.lim.vec <- y.vec - 2.58*sdy.vec
plot(y.vec ~ x.vec)
points(lower.lim.vec ~ x.vec)
points(upper.lim.vec ~ x.vec)



a <- 0.2
b <- 1.8

y=0.2*x^1.8
y/0.2=x^1.8
y^(1/1.8)*(1/0.2)^(1/1.8)=x
(inv.a <- (1/a)^(1/b))
(inv.b <- 1/b)


xy.func <- function(x){inv.a*x^inv.b}
xsdy.func <- function(x){1.2*x}

new.x.vec <- xy.func(y.vec)
new.sdx.vec <- xsdy.func(y.vec)
new.x.upper.lim.vec <- new.x.vec + 2.58*new.sdx.vec
new.x.lower.lim.vec <- new.x.vec - 2.58*new.sdx.vec
plot(new.x.vec ~ y.vec)
points(new.x.lower.lim.vec ~ y.vec)
points(new.x.upper.lim.vec ~ y.vec)




##################################################
# develop and test density transformation function
##################################################

x.vec <- rlnorm(200, 3.2, 0.3)
x.d.vec <- dlnorm(x.vec, 3.2, 0.3)
n.steps = 1000
hist(x.vec, breaks=30, freq=F)
curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
xy.func <- function(x){0.1*x^1.8}
xsdy.func <- function(x){1.2*x}



transform_density <- function(x.vec, x.d.vec, xy.func=function(x){1*x}, n.steps=1000){

  # Sort
  order.vec <- order(x.vec)
  x.vec <- x.vec[order.vec]
  x.d.vec <- x.d.vec[order.vec]

  # Derive the CDF
  x.cdf.vec <- cumsum(x.d.vec)
  x.cdf.vec <- x.cdf.vec/x.cdf.vec[length(x.cdf.vec)]
  x.cdf.fun <- approxfun(x=x.vec, y=x.cdf.vec, rule=2)

  x.pdf.fun <- approxfun(x=x.vec, y=x.d.vec, rule=2)

  plot(x.d.vec~x.vec)
  plot(x.cdf.vec~x.vec)

  # Define a vector of all x values with a certain step width
  x.steps.vec <- seq(from=min(x.vec), to=max(x.vec), length.out=n.steps)

  # Calculate the probability of each x value
  prob.vec <- x.cdf.fun(x.steps.vec)
  plot(prob.vec~x.steps.vec)

  d.vec <- x.pdf.fun(x.steps.vec)
  plot(d.vec~x.steps.vec)

  # Predict a y value for each x value
  y.steps.vec <- xy.func(x.steps.vec)

  plot(prob.vec~y.steps.vec)
  plot(d.vec~y.steps.vec)

  # Approximate the y CDF
  y.cdf.fun <- approxfun(x=y.steps.vec, y=prob.vec, rule=2)

  y.d.vec <- c(0, diff(prob.vec))
  plot(y.d.vec~y.steps.vec)
  plot(x.d.vec~y.steps.vec)
  plot(x.d.vec~x.vec)

  return(result.list)
}




x.vec <- rlnorm(200, 3.2, 0.3)
x.d.vec <- dlnorm(x.vec, 3.2, 0.3)
n.steps = 1000
hist(x.vec, breaks=30, freq=F)
curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
xy.func <- function(x){0.1*x^1.8}
xsdy.func <- function(x){1.2*x}



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
  x.steps.pdf.vec <- x.pdf.fun(x.steps.vec)
  # Calculate a y value for each x.steps value using the given x-y-function
  y.steps.vec <- xy.func(x.steps.vec)
  # Derive a function that approximates the PDF for y with linear
  # interpolation between given density values. As density for each y value,
  # the density of the corresponding x value is used
  y.pdf.fun <- approxfun(x=y.steps.vec, y=x.steps.pdf.vec, rule=2)
  # Calculate the integral under the PDF of y to correct the the density
  # values and refit the approxfun to ensure a total probability (integral)
  # of 1
  y.pdf.integral <- integrate(f=y.pdf.fun, lower=min(y.vec), upper=max(y.vec))
  y.steps.pdf.vec <- x.steps.pdf.vec / y.pdf.integral[[1]]
  y.pdf.fun <- approxfun(x=y.steps.vec, y=y.steps.pdf.vec, rule=2)
  # Make a list of results for returning
  result.list <- list()
  result.list$y <- y.vec
  result.list$x.steps <- x.steps.vec
  result.list$y.steps <- y.steps.vec
  result.list$x.steps.pdf <- x.steps.pdf.vec
  result.list$y.steps.pdf <- y.steps.pdf.vec
  result.list$x.pdf.fun <- x.pdf.fun
  result.list$y.pdf.fun <- y.pdf.fun
  return(result.list)
}


my.x.vec <- rlnorm(200, 3.2, 0.3)
my.x.d.vec <- dlnorm(my.x.vec, 3.2, 0.3)
sum(my.x.d.vec)
my.n.steps = 1000
hist(my.x.vec, breaks=30, freq=F)
curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
points(my.x.d.vec ~ my.x.vec, col="red")
my.xy.func <- function(x){0.1*x^1.8}
my.xsdy.func <- function(x){1.2*x}

transform.list <- transform_pdf_with_function(x.vec=my.x.vec, x.d.vec=my.x.d.vec,
                                         xy.func=my.xy.func, n.steps=my.n.steps)


plot(transform.list$y ~ my.x.vec)
plot(transform.list$x.steps.pdf ~ transform.list$x.steps)
plot(transform.list$y.steps.pdf ~ transform.list$y.steps)

hist(transform.list$y, breaks=30, freq=F)
new.y.steps.vec <- 1:150
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="blue")


sum(new.y.pdf.vec)
new.y.pdf.vec2 <- new.y.pdf.vec/sum(new.y.pdf.vec)
lines(new.y.pdf.vec2 ~ new.y.steps.vec, col="green")


test.x.vec <- 1:50
test.y.vec <- test.list$y.pdf.fun(test.x.vec)

plot(test.y.vec ~ test.x.vec)

test.list[[1]]

test.list[[4]]


#################
# Include SD
################



x.vec <- rlnorm(200, 3.2, 0.3)
x.d.vec <- dlnorm(x.vec, 3.2, 0.3)
n.steps = 1000
hist(x.vec, breaks=30, freq=F)
curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
xy.func <- function(x){0.1*x^1.8}
xsdy.func <- function(x){1.2*x}


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

my.x.vec <- rlnorm(200, 3.2, 0.3)
my.x.d.vec <- dlnorm(my.x.vec, 3.2, 0.3)
sum(my.x.d.vec)
my.n.steps = 1000
hist(my.x.vec, breaks=30, freq=F)
curve(dlnorm(x, 3.2, 0.3), add=T, col="blue")
points(my.x.d.vec ~ my.x.vec, col="red")
my.xy.func <- function(x){0.1*x^1.8}
my.xsdy.func <- function(x){0.5*x}
my.xsdy.func <- function(x){0.1*x^1.8}
my.ysdy.func <- function(y){0.1*y^1.8}

transform.list <- transform_pdf_with_function(x.vec=my.x.vec,
                                              x.d.vec=my.x.d.vec,
                                              xy.func=my.xy.func,
                                              xsdy.func=my.xsdy.func,
                                              n.steps=my.n.steps)


plot(transform.list$y ~ my.x.vec)
plot(transform.list$x.steps.pdf ~ transform.list$x.steps)
plot(transform.list$y.steps.pdf ~ transform.list$y.steps)

hist(transform.list$y, breaks=30, freq=F)
new.y.steps.vec <- 1:150
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="blue")

pred.y <- my.xy.func(my.x.vec) +
  rnorm(n=length(my.x.vec), mean=0, sd=my.xsdy.func(my.x.vec))
hist(pred.y, breaks=30, freq=F)
new.y.steps.vec <- 1:150
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="green")

new.y.steps.vec <- -200:400
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="green")


# Check ysdy function

transform.list <- transform_pdf_with_function(x.vec=my.x.vec,
                                              x.d.vec=my.x.d.vec,
                                              xy.func=my.xy.func,
                                              xsdy.func=my.xsdy.func,
                                              ysdy.func=my.ysdy.func,
                                              n.steps=my.n.steps)

transform.list <- transform_pdf_with_function(x.vec=my.x.vec,
                                              x.d.vec=my.x.d.vec,
                                              xy.func=my.xy.func,
                                              ysdy.func=my.ysdy.func,
                                              n.steps=my.n.steps)


plot(transform.list$y ~ my.x.vec)
plot(transform.list$x.steps.pdf ~ transform.list$x.steps)
plot(transform.list$y.steps.pdf ~ transform.list$y.steps)

hist(transform.list$y, breaks=30, freq=F)
new.y.steps.vec <- 1:150
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="blue")

pred.y <- my.xy.func(my.x.vec) +
  rnorm(n=length(my.x.vec), mean=0, sd=my.xsdy.func(my.x.vec))
hist(pred.y, breaks=30, freq=F)
new.y.steps.vec <- 1:150
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="green")

new.y.steps.vec <- -200:400
new.y.pdf.vec <- transform.list$y.pdf.fun(new.y.steps.vec)
lines(new.y.pdf.vec ~ new.y.steps.vec, col="green")





















































