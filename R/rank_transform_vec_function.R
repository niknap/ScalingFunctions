#' Rank transformation based on a vector of target values
#'
#' Rank transform a dataset, i.e., for each input value provide an output value
#' from a given target vector. The target vector will be rearranged such that
#' the ranking of values corresponds the ranking of the input vector values.
#' @param x Input vector
#' @param y Target vector
#' @return Vector of transformed values
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

rank_transform_vec <- function(x, y){
  # Check for consistent length of x and y vectors
  if(length(x) == length(y)){
    # Sort y by size
    sorted.y <- sort(y)
    # Reorder y by the order of x
    reordered.y <- sorted.y[order(x)]
    return(reordered.y)
  }else{
    warning("Lengths of x and y are different")
  }
}


