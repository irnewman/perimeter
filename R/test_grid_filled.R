
# returns TRUE if the grid is properly filled, FALSE if a hole is found

  # THIS ONLY WORKS WITH A SINGLE CELL HOLE, needs to be fixed further
    # but not a priority, for now, can visually inspect

test_grid_filled <- function(matrix)
{

  found_hole <- FALSE

  for (i in 2:(nrow(matrix)-1)) {
    for (j in 2:(ncol(matrix)-1)) {
      if (matrix[i, j] == 0) {
        empty_cell_neighbours <- count_neighbours(
          matrix, i, j, (nrow(matrix)-1), (ncol(matrix)-1))
        if (empty_cell_neighbours == 4) {
          found_hole <- TRUE
        }
      }
    }
  }
  return(!found_hole)
}
