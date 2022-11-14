# rows or columns

# Error in if (nrow(matrix) != ncol(matrix)) { : argument is of length zero
  # what function is this from??

find_eligible_vecs <- function(matrix, side = "random", type)
{
  if (side == "random") {
    side <- sample(c("left", "right", "top", "bottom"), 1)
  }

  if (side == "left" | side == "right") {
    loops <- nrow(matrix)
    direction <- "row"
  }
  if (side == "top" | side == "bottom") {
    loops <- ncol(matrix)
    direction <- "column"
  }

  eligible_vecs <- c()

  # check all rows that have at least one "1" in them
  for (i in 1:loops) {

    vec_full <- FALSE

    if (direction == "row") {
      # check if any 1's in the row
      current_vec <- matrix[i, ]
      num_of_vec_boxes <- length(which(current_vec == 1))

      if (side == "right"
          & current_vec[ncol(matrix)] == 1) {
        vec_full <- TRUE
      }

      if (side == "left"
          & current_vec[1] == 1) {
        vec_full <- TRUE
      }
    }

    if (direction == "column") {
      # check if any 1's in the row
      current_vec <- matrix[, i]
      num_of_vec_boxes <- length(which(current_vec == 1))

      if (side == "top"
          & current_vec[1] == 1)  {
        vec_full <- TRUE
      }

      if (side == "bottom"
          & current_vec[nrow(matrix)] == 1) {
        vec_full <- TRUE
      }
    }

    # find index of side in the current row or column
    if (side == "left" | side == "top") {
      edge_index <- match(1, current_vec)
    }
    if (side == "right" | side == "bottom") {
      edge_index <- tail(which(current_vec == 1), 1)
    }

    # if there are 1's in the row and the next index isn't a 0 in that row
    if (num_of_vec_boxes > 0 & type == "subtraction") {
      if (side == "left" | side == "top") {
        if (current_vec[edge_index + 1] == 1) {
          eligible_vecs <- c(eligible_vecs, i)
        }
      }
      if (side == "right" | side == "bottom") {
        if (current_vec[edge_index - 1] == 1) {
          eligible_vecs <- c(eligible_vecs, i)
        }
      }
    }

    # if there are 1's in the row and row not full, add to eligible rows vector
    if (num_of_vec_boxes > 0 & type == "addition" & vec_full == FALSE) {
      eligible_vecs <- c(eligible_vecs, i)
    }
  }

  return(eligible_vecs)
}
