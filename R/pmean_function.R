
# Calculate power mean
pmean <- function(x, p=1, na.rm=T, zero.rm=F){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  if(zero.rm == T){
    x <- x[x != 0]
  }
  n <- length(x)
  # If p is 0 return the geometric mean,
  # which can be proven to be the limit
  # of the power mean in that case.
  # If p is + or - infinite the result
  # is the minimum or maximum of the
  # input vector.
  if(p == 0){
    return(prod(x^(1/n)))
  }else if(p == -Inf){
    return(min(x))
  }else if(p == Inf){
    return(max(x))
  }else{
    return((sum(x^p)/n)^(1/p))
  }
}
