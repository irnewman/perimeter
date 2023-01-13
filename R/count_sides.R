
# method will count corners actually, but sides = corners

count_sides <- function(matrix)
{
  corners <- 0
  # loop through each cell in matrix
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {

      # if cell = 0, look for internal corners
      if (matrix[i, j] == 0) {
        current_cell <- count_neighbours(matrix, i, j)

        if (current_cell == 4) {
          print("error: count sides found a hole")
          plot_matrix(matrix)
        } else if (current_cell == 3) {
          # found 2 internal corners
          corners <- corners + 2
        } else if (current_cell == 2) {
          # check if neighbours are on opposite sides; if so, not a corner
          current_edges <- find_edges(matrix, i, j)
          if (identical(current_edges, c("left", "right"))
              | identical(current_edges, c("top", "bottom"))) {
            corners <- corners
          } else {
            # found 1 internal corner
            corners <- corners + 1
          }
        } else {
          corners <- corners
        }
      }

      # if cell = 1, look for external corners
      if (matrix[i, j] == 1) {
        current_cell <- count_neighbours(matrix, i, j)

        if (current_cell == 0) {
          print("error: found a floating piece")
        } else if (current_cell == 1) {
          # found 2 external corners
          corners <- corners + 2
        } else if (current_cell == 2) {
          # check if neighbours are on opposite sides; if so, not a corner
          current_edges <- find_edges(matrix, i, j)
          if (identical(current_edges, c("left", "right"))
              | identical(current_edges, c("top", "bottom"))) {
            corners <- corners
          } else {
            # found 1 external corner
            corners <- corners + 1
          }
        } else {
          corners <- corners
        }
      }
    }
  }
  return(corners)
}
