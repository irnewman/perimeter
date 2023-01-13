
add_n_boxes <- function(matrix, n) {

  # add check for if N exceeds empty space
  total_area <- nrow(matrix) * ncol(matrix)
  if ((sum(matrix) + n) >= total_area) {
    return("unable to add N boxes: insufficient area")
  }

  for (i in 1:n) {
    matrix <- add_box(matrix)
  }

  return(matrix)
}
