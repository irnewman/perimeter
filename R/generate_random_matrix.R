

generate_random_matrix <- function(area, grid_size = 10)
{
  passed <- FALSE

  while (!passed) {

    v_random <- generate_random_vector(area, grid_size)
    random_matrix <- fill_matrix(grid_size, v_random)

    if (!test_grid_fail(random_matrix)) {
      # if not failed, then passed and finish
      passed <- TRUE
    }
  }


  return(random_matrix)
}
