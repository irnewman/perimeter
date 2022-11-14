
# general: takes matrix, vector and fills as centered as possible

fill_matrix <- function(grid_size, v = c())
{
  # total grid space, not shape
  matrix <- data.frame(matrix(nrow = grid_size, ncol = grid_size))

  # select padding to center as much as possible
  width <- length(v)
  height <- max(v)
  padding <- determine_padding(width, height, grid_size)

  if (length(v) > 0) {  # use vector to fill as well
    # add rows to left/right
    v_padded <- c(
      rep(0, padding$left),
      v,
      rep(0, padding$right)
    )

    # j = horizontally, left to right
    for (j in 1:ncol(matrix)) {
      matrix[, j] <- c(
        rep(0, padding$top),
        rep(0, nrow(matrix) - (
          padding$top + v_padded[j] + padding$bottom)),
        rep(1, v_padded[j]),
        rep(0, padding$bottom))
    }
  } else {
    for (i in 1:nrow(matrix)) {  # i = vertically, top to bottom
      if ( (i <= padding$top) | (i > grid_size - padding$bottom)) {
        matrix[i, ] <- c(rep(0, grid_size))
      } else {
        matrix[i, ] <- c(
          rep(0, padding$left),
          rep(1, (grid_size - (padding$left+padding$right))),
          rep(0, padding$right))
      }
    }
  }

  return(matrix)
}

