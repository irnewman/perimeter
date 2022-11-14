# add rows/columns to a matrix
# function to add rows/columns of 0's
# used for calculating complexity

increase_matrix <- function(matrix, top = 0, bottom = 0, left = 0, right = 0)
{
  m <- matrix
  # add rows above
  if (top > 0) {
    for (i in 1:top) {
      m <- rbind(rep(0, ncol(m)),
                 m)
    }
  }

  # add row below
  if (bottom > 0) {
    for (j in 1:bottom) {
      m <- rbind(m,
                 rep(0, ncol(m)))
    }
  }

  # col left
  if (left > 0) {
    for (k in 1:left) {
      m <- cbind(rep(0, nrow(m)),
                 m)
      colnames(m)[1] <- paste0("c", k)
      # this is 1 because it adds to left each time
    }
  }

  # col right
  if (right > 0) {
    for (l in 1:right) {
      m <- cbind(m,
                 rep(0, nrow(m)))
      colnames(m)[ncol(m)] <- paste0("r", l)
      # this is ncol(m) because it adds to end of right side each time
    }
  }

  return(m)
}


