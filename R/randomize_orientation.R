
randomize_orientation <- function(matrix)
{
  # flip matrix, yes or no
  flip <- sample(c(0,1), 1)
  if (flip == 1) {
    matrix <- flip_matrix(matrix)
  }

  # rotate matrix random number of times
  rotations <- sample(c(0, 1, 2, 3), 1)
  for (i in 1:rotations) {
    matrix <- as.data.frame(rotate_matrix(matrix, clockwise = TRUE))

  }

  return(matrix)
}
