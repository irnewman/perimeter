
flip_matrix <- function(matrix, direction = "horizontal")
{
  if (direction == "horizontal") {
    new_cols <- rev(colnames(matrix))
    flipped <- matrix[, new_cols, drop = FALSE]
  } else {
    return(print("error"))
  }

  return(flipped)
  # add vertical, y=x, any others later
}
