# Calculate the mean of a GB2 distribution
mean_of_gb2 <- function(a, b, p, q){
  result <- (b*beta(p+1/a, q-1/a))/beta(p, q)
  return(result)
}
