
find_width <- function(matrix)
{
  return(length(which(colSums(matrix) != 0)))
}
