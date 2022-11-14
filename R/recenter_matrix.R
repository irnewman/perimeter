# BUG: sometimes returns a smaller grid than matrix was, not sure why

recenter_matrix <- function(matrix)
{
  reduced <- trim_matrix(matrix)

  h <- colSums(matrix)
  h_left <- as.integer(min(which(h != 0)) - 1)
  h_right <- ncol(matrix) - (max(which(h != 0)))
  h_sum <- h_left + h_right
  h_diff <- abs(h_left - h_right)
  horizontally_centered <- TRUE
  if (h_diff > 1) {
    horizontally_centered <- FALSE
  }

  v <- rowSums(matrix)
  v_top <- as.integer(min(which(v != 0)) - 1)
  v_bottom <- nrow(matrix) - (max(which(v != 0)))
  v_sum <- v_top + v_bottom
  v_diff <- abs(v_top - v_bottom)
  vertically_centered <- TRUE
  if (v_diff > 1) {
    vertically_centered <- FALSE
  }

  if (!horizontally_centered) {
    h_recenter <- sample(c(floor(h_sum/2),
                           floor(h_sum/2) + h_sum%%2))
    h_left <- h_recenter[1]
    h_right <- h_recenter[2]
  }

  if (!vertically_centered) {
    v_recenter <- sample(c(floor(v_sum/2),
                           floor(v_sum/2) + v_sum%%2))
    v_top <- v_recenter[1]
    v_bottom <- v_recenter[2]
  }
  centered <- increase_matrix(reduced,
                              top = v_top,
                              bottom = v_bottom,
                              left = h_left,
                              right = h_right)

  if (nrow(centered) != nrow(matrix) | ncol(centered) != ncol(matrix)) {
    print("failed to recenter properly")
    print(paste0("v_top: ", v_top))
    print(paste0("v_bottom: ", v_bottom))
    print(paste0("h_left: ", h_left))
    print(paste0("h_right: ", h_right))
    return(matrix)
  }

  return(centered)
}

