
calculate_perimeter <- function(matrix)
{
  perimeter <- 0

  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (matrix[i, j] == 1) {
        perimeter <- perimeter + (4 - count_neighbours(
          matrix, i, j))
      }
    }
  }
  return(perimeter)
}
