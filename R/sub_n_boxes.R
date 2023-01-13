

sub_n_boxes <- function(matrix, n) {

  # add check for if N reduces below 1\
  if ((sum(matrix) - n) <= 1) {
    return("unable to sub N boxes: removes all boxes")
  }

  for (i in 1:n) {
    matrix <- sub_box(matrix)
  }

  return(matrix)
}
