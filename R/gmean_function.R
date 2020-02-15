# Calculate geometric mean
gmean <- function(x, na.rm=T, zero.rm=F){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  if(zero.rm == T){
    x <- x[x != 0]
  }
  n <- length(x)
  return(prod(x^(1/n)))
}
