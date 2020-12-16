#' Distribute trees with spacing
#'
#' Distribute trees in space with a size dependent minimal distance
#' @param d.vec Vector of tree stem diameters
#' @param minx Minimal X-coordinate of the area of interest
#' @param maxx Maximal X-coordinate of the area of interest
#' @param miny Minimal Y-coordinate of the area of interest
#' @param maxy Maximal Y-coordinate of the area of interest
#' @param spacing.factor Factor that specifies the minimal possible distance to the next neighbor tree as a function of tree stem diameter
#' @return data.table of trees with positions
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

distribute_trees_with_spacing <- function(d.vec, minx=0, maxx=100, miny=0, maxy=100, spacing.factor=5){
  require(data.table)
  # Remember the original order of trees
  order.vec <- order(d.vec, decreasing=T)
  # Sort from largest to smallest tree
  sorted.d.vec <- sort(d.vec, decreasing=T)
  # Expand a table of all possible positions
  points.dt <- data.table(expand.grid(X=(minx+1):maxx, Y=(miny+1):maxy))
  # Prepare the tree table
  n <- length(sorted.d.vec)
  trees.dt <- data.table()
  # Loop from largest to smallest tree
  my.percent <- 0
  for(i in 1:n){
    my.d <- sorted.d.vec[i]
    # Sample a random row from the table of all available points
    pt.idx <- sample(x=nrow(points.dt), size=1)
    # Position the tree at these random coordinates
    my.x <- points.dt[pt.idx, X] - runif(1, min=0, max=1)
    my.y <- points.dt[pt.idx, Y] - runif(1, min=0, max=1)
    my.tree <- matrix(c(X=my.x, Y=my.y, D=my.d), nrow=1)
    # Add the tree with its position to the tree table
    trees.dt <- rbind(trees.dt, my.tree)
    # Identify points which fall in circle around the tree
    # The circle radius is a linear function of tree DBH
    # multiplied with a custom spacing factor
    my.r <- spacing.factor * my.d
    in.circle.vec <- ifelse((points.dt$X - my.x)^2 + (points.dt$Y - my.y)^2 <= my.r^2, T, F)
    # Remove the points in the circle around the tree from the table of available points
    points.dt <- points.dt[!in.circle.vec,]
    # Show progress
    my.new.percent <- round(100*i/n)
    if(my.new.percent > my.percent){
      my.percent <- my.new.percent
      print(paste0(my.percent, "%"))
    }
  }
  # Add header
  setnames(trees.dt, old=names(trees.dt), new=c("X", "Y", "D"))
  # Restore original order of trees
  trees.dt <- trees.dt[match(1:nrow(trees.dt), order.vec),]
  return(trees.dt)
}
