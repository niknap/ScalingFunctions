
x=1:6
distr="norm"

pfun <- pnorm(x, 2, 5)

rank_transform_distr <- function(x, distr="norm", ...){
  # Derive the empirical cumulative distribution function (ECDF) of the
  # input data
  input.ecdf <- ecdf(x)
  # For each element in x calculate which quantile of the ECDF it is
  quantiles.vec <- input.ecdf(x)
  # Get the inverse cumulative distribution function (ICDF), also called
  # quantile function, of the output data
  output.icdf <- get(paste0("q", distr), mode="function")
  # Calculate the output value from each input's quantile
  output.vec <- output.icdf(quantiles.vec, ...)
  return(output.vec)
}


test.x <- 1:100
test.y <- rank_transform_distr(x=test.x, distr="norm", mean=60, sd=2)
hist(test.x)
hist(test.y)

test.x <- rnorm(100, 60, 40)
test.y <- rank_transform_distr(x=test.x, distr="gamma", shape=0.5, rate=1)
hist(test.x)
hist(test.y)

test.x <- rnorm(100, 60, 40)
test.y <- rank_transform_distr(x=test.x, distr="trunc", mean=60, sd=20,
                               spec="norm", a=40, b=80)
hist(test.x)
hist(test.y)


require(raster)
test.x.mx <- matrix(runif(1000000), nrow=1000, ncol=1000)
test.x.ras <- raster(test.x.mx)
plot(test.x.ras)

test.y.ras <- test.x.ras
test.y.ras[] <- rank_transform_distr(x=test.x.ras[], distr="trunc", mean=60,
                                     sd=5, spec="norm", a=40, b=80)
plot(test.y.ras)
hist(test.x.ras)
hist(test.y.ras)



x=1:6
x=c(3,3,3,1,2,4)
order(x)
distr="norm"

rank_transform_vec <- function(x, y){
  # Check for consistent length of x and y vectors
  if(length(x) == length(y)){
    # Sort y by size
    sorted.y <- sort(y)
    # Reorder y by the order of x
    reordered.y <- sorted.y[order(x)]
    return(reordered.y)
  }else{
    warning("Lengths of vectors are different")
  }
}

require(raster)
test.x.mx <- matrix(runif(100), nrow=10, ncol=10)
test.x.ras <- raster(test.x.mx)
plot(test.x.ras)

bimodal.y.vec <- c(rnorm(40, 10, 3), rnorm(60, 30, 3))
hist(bimodal.y.vec)

test.y.ras <- test.x.ras
test.y.ras[] <- rank_transform_vec(x=test.x.ras[], bimodal.y.vec)
plot(test.x.ras)
plot(test.y.ras)
hist(test.x.ras)
hist(test.y.ras)


test.x.mx <- matrix(runif(10000), nrow=100, ncol=100)
test.x.ras <- raster(test.x.mx)
plot(test.x.ras)

bimodal.y.vec <- c(rnorm(4000, 10, 3), rnorm(6000, 30, 3))
hist(bimodal.y.vec)

test.y.ras <- test.x.ras
test.y.ras[] <- rank_transform_vec(x=test.x.ras[], bimodal.y.vec)
plot(test.x.ras)
plot(test.y.ras)
hist(test.x.ras)
hist(test.y.ras)





