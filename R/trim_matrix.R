
# function to remove all rows/columns with only 0's
# used for calculating complexity

trim_matrix <- function(matrix)
{
  return(matrix[(rowSums(matrix) > 0), colSums(matrix) > 0])
}
