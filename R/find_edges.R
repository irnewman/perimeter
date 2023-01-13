
# this function is essentially find neighbours, could re-name

find_edges <- function(matrix, i, j)
{
  n <- nrow(matrix)
  m <- ncol(matrix)
  edges <- c()

  # above: if i = 1, you can't look above
  if ( (i > 1 && matrix[i-1, j]) == 1) {
    edges <- c(edges, "top")
  }

  # left: if j = 1, you can't look left
  if ( (j > 1 && matrix[i, j-1]) == 1) {
    edges <- c(edges, "left")
  }

  # below: if i = nrows, you can't look below
  if ( (i < n && matrix[i+1, j]) == 1) {
    edges <- c(edges, "bottom")
  }


  # right: if j = ncols, you can't look right
  if ( (j < m && matrix[i, j+1]) == 1) {
    edges <- c(edges, "right")
  }


  if (length(edges) == 0) {
    return("none")
  } else {
    return(edges)
  }

}
