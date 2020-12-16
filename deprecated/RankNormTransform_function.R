# Function for rank transformation, which converts the data
# of any distribution given in vector x into a standard normal
# distribution with mean = 0 and sd = 1
RankNormTransform <- function(x){
  # Calculate the normal score of each value, i.e., if the values in x
  # are ranked by size, which value on the normal distribution would
  # correspond to each value in x
  normal.scores <- qqnorm(x, plot.it=F)$x
  return(normal.scores)
}
