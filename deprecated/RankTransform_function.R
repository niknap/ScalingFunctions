# Function for arbitrary rank transformation, which sorts the
# newval.vec in the order of the ranks of the order.vec
# vector
RankTransform <- function(order.vec, newval.vec){
  if(length(order.vec) == length(newval.vec)){
    ranks.vec <- rank(order.vec, ties.method="random")
    sorted.newval.vec <- sort(newval.vec)
    reordered.newval.vec <- sorted.newval.vec[ranks.vec]
    return(reordered.newval.vec)
  }else{
    warning("Lengths of vectors are different")
  }
}
