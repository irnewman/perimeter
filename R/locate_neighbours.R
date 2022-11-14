

locate_neighbours <- function(matrix, i, j)
{
  n <- nrow(matrix)
  m <- ncol(matrix)
  neighbours <- data.frame(matrix(ncol=2, nrow=0))

  # above: if i = 1, you can't look above
  if ( (i > 1 && matrix[i-1, j]) == 1) {
    neighbours <- rbind(neighbours, c(i-1, j))
  }

  # left: if j = 1, you can't look left
  if ( (j > 1 && matrix[i, j-1]) == 1) {
    neighbours <- rbind(neighbours, c(i, j-1))
  }

  # below: if i = nrows, you can't look below
  if ( (i < n && matrix[i+1, j]) == 1) {
    neighbours <- rbind(neighbours, c(i+1, j))
  }


  # right: if j = ncols, you can't look right
  if ( (j < m && matrix[i, j+1]) == 1) {
    neighbours <- rbind(neighbours, c(i, j+1))
  }

  return(neighbours)
}
