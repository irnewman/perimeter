
# check if the shape is centered within the area

is_centered <- function(matrix)
{
  h <- colSums(matrix)
  h_left <- as.integer(min(which(h != 0)) - 1)
  h_right <- nrow(matrix) - (max(which(h != 0)))
  horizontally_centered <- TRUE
  if (abs(h_left - h_right) > 1) {
    horizontally_centered <- FALSE
  }

  v <- rowSums(matrix)
  v_top <- as.integer(min(which(v != 0)) - 1)
  v_bottom <- nrow(matrix) - (max(which(v != 0)))
  vertically_centered <- TRUE
  if (abs(v_top - v_bottom) > 1) {
    vertically_centered <- FALSE
  }

  if (vertically_centered == FALSE | horizontally_centered == FALSE) {
    #print(paste0("vertical centering: ", vertically_centered))
    #print(paste0("horizontal centering: ", horizontally_centered))
    return(FALSE)
  } else {
    return(TRUE)
  }
}






#
# test_centerable <- function(matrix)
# {
#
# }
#
#
#

