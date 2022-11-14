
# use change index to specify which ROW/COL to make the change, otherwise random
change_box <- function(matrix, side = "random", operation, change_index = 0)
{
  # if side is random, select a side to change
  if (side == "random") {
    side <- sample(c("left", "right", "top", "bottom"), 1)
  }
  if (side == "left" | side == "right") {
    direction <- "row"
  }
  if (side == "top" | side == "bottom") {
    direction <- "column"
  }

  # return if no eligible rows found
  eligible_vecs <- find_eligible_vecs(matrix, side, operation)
  if (length(eligible_vecs) == 0) {
    return(matrix)
  }

  # select which index of all possible to change unless specified in arguments
  if (change_index != 0) {
    vec_index <- change_index
  } else if (length(eligible_vecs) > 1) {
    vec_index <- sample(eligible_vecs, 1)
  } else {
    vec_index <- eligible_vecs
  }
  if (direction == "row") {
    vec_to_change <- matrix[vec_index, ]
  }
  if (direction == "column") {
    vec_to_change <- matrix[, vec_index]
  }

  # return index that should change, add: 0 to 1, sub: 1 to 0
  box_to_change <- select_box(vec_to_change, side, operation)

  if (operation == "addition" & direction == "row") {
    matrix[vec_index, box_to_change] <- 1
  }
  if (operation == "subtraction" & direction == "row") {
    matrix[vec_index, box_to_change] <- 0
  }
  if (operation == "addition" & direction == "column") {
    matrix[box_to_change, vec_index] <- 1
  }
  if (operation == "subtraction" & direction == "column") {
    matrix[box_to_change, vec_index] <- 0
  }

  # NOTE: removing this for now, too, since it doesn't work fully
  # matrix_passed <- FALSE
  # while (!matrix_passed) {
  #   #matrix_passed <- test_grid_filled(matrix)
  #   matrix_passed <- TRUE
  # }

  return(matrix)
}
