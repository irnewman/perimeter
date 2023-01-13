
# add a random rotation to this function to make it better randomized

# could also make a function to generate a random area (from parameter matrix)

generate_random_matrix <- function(area = 0, grid_size = 6)
{
  if (area == 0) {
    area_bounds <- ((grid_size^2)/4):((grid_size^2)/2)
    area <- sample(area_bounds, 1)
  }

  passed <- FALSE

  while (!passed) {

    v_random <- generate_random_vector(area, grid_size)
    random_matrix <- fill_matrix(grid_size, v_random)

    if (!test_grid_fail(random_matrix)) {
      # if not failed, then passed and finish
      passed <- TRUE
    }
  }


  return(randomize_orientation(random_matrix))
}
