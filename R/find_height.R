
find_height <- function(matrix)
{
  return(length(which(rowSums(matrix) != 0)))
}
