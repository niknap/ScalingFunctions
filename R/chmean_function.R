# Calculate contraharmonic mean
chmean <- function(x, na.rm=T){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  x <- x[x != 0]
  return(sum(x^2) / sum(x))
}
