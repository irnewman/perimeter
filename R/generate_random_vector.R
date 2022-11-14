
# new
# 1 pick a length, max grid size, min half of grid size
# 2 determine padding function




generate_random_vector <- function(area, grid_size)
{

  p <- as.integer(grid_size/4)

   # determine if rows in left padding are filled
  left_padded <- p - sample_placement(grid_size, p, type = "padding")
  right_padded <- p - sample_placement(grid_size, p, type = "padding")


  v_total <- 0
  b_total <- 0


  # ADD TO WHILE
  # check that there is only 1 connected shape
  # check that no values exceed the shape size (b + v > gridsize any index)

  while ((v_total) != area) {

    # make bottom vector of 1's to add below v  (use p - bottom[i])
    b_values <- replicate(grid_size,
                          (sample_placement(grid_size, p, type = "padding")))
    bottom <- rep(p, grid_size) - b_values
    b_total <- sum(bottom)

    # make shape vector
    v <- replicate((grid_size - (left_padded+right_padded)),
                   (sample_placement((grid_size-1),  # not so tall
                                     p, type = "middle")))
    v_total <- sum(v)

    # make a test vector for checks
    v_test <- c(
      rep(0, left_padded),
      v,
      rep(0, right_padded)
    )

    # fix any out of bounds errors
    if ( any( (v_test + bottom) > grid_size)) {
      v_total <- -1
    }

    # # if grid in multiple pieces :: if the grid fails, re-do
    # if (test_grid_fail(fill_matrix(v_test, bottom))) {
    #   v_total <- -1
    # }
  }

  v_random <- c(
    rep(0, left_padded),
    v,
    rep(0, right_padded)
  )

  #print(sum(v_random))
  return(v_random)
}
