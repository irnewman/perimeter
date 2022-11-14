
compute_complexity <- function(matrix)
{
  # reduce the shape to minimum grid area (removes empty rows/cols)
  reduced <- trim_matrix(matrix)
  max_rows <- nrow(reduced)
  max_cols <- ncol(reduced)
  max_grid <- max(max_rows, max_cols)

  # set a minimum size as half of max, rounded down
  minimum_size <- floor(max_grid/2)

  # make list of 0-only rows for each side
  grid_list <- list()

  # generate exhaustive list of left/right/top/bottom offsets for shapes
  row_count <- max_rows
  while (row_count >= minimum_size) {
    row_diff <- max_rows - row_count
    col_count <- max_cols
    while (col_count >= minimum_size) {
      col_diff <- max_cols - col_count
      if (row_diff == 0 & col_diff == 0) {
        list_entry <- c(top = 0, bottom = 0, left = 0, right = 0)
        grid_list[[1]] <- list_entry
      } else {
        for (i in 1:(row_diff+1)) {
          t <- i-1
          b <- row_diff - t
          for (j in 1:(col_diff+1)) {
            l <- j-1
            r <- col_diff - l
            list_entry <- c(top = t, bottom = b, left = l, right = r)
            grid_list[[length(grid_list)+1]] <- list_entry
          }
        }
      }
      col_count <- col_count - 1
    }
    row_count <- row_count - 1
  }

  # make a data frame of shapes to test as best-fitting baseline
  test_shapes <- do.call(rbind.data.frame, grid_list)
  colnames(test_shapes) <- c("top", "bottom", "left", "right")
  test_shapes$width <- max_cols - (test_shapes$left + test_shapes$right)
  test_shapes$height <- max_rows - (test_shapes$top + test_shapes$bottom)
  test_shapes$max_grid <- max_grid
  test_shapes$area_shape <- sum(reduced)
  test_shapes$area_test <- abs((test_shapes$width * test_shapes$height))
  test_shapes$area_diff <- abs((test_shapes$area_shape - test_shapes$area_test))
  test_shapes$index <- c(1:nrow(test_shapes))
  rownames(test_shapes) <- c(1:nrow(test_shapes))

  # test the shape that fills the full grid
  test_full_grid <- generate_matrix(width = test_shapes$width[1],
                                    height = test_shapes$height[1],
                                    grid_size = test_shapes$max_grid[1],
                                    padding = "specified",
                                    top = test_shapes$top[1],
                                    bottom = test_shapes$bottom[1],
                                    left = test_shapes$left[1],
                                    right = test_shapes$right[1]
                                    )
  comp_full_grid <- count_differences(test_full_grid,
                                      reduced)

  # # smaller list of area values that are similar to the tested shape's area
  # similar_area <- test_shapes[test_shapes$area_diff <= (sum(reduced) * 0.1), ]
  # print(similar_area)
  # # test a random shape that is close in area to the reduced
  # random_index <- sample(similar_area$index, 1)
  # test_rand_grid <- generate_matrix(width = test_shapes$width[random_index],
  #                                   height = test_shapes$height[random_index],
  #                                   grid_size =
  #                                     test_shapes$max_grid[random_index],
  #                                   padding = "specified",
  #                                   top = test_shapes$top[random_index],
  #                                   bottom = test_shapes$bottom[random_index],
  #                                   left = test_shapes$left[random_index],
  #                                   right = test_shapes$right[random_index])
  # comp_rand_grid <- count_differences(test_rand_grid,
  #                                     reduced)

  # record lowest difference so far, to be updated
  #benchmark <- min(comp_full_grid, comp_rand_grid)
  benchmark <- min(comp_full_grid)
  benchmark_fits <- sum(reduced)

  # update a list of theoretically best candidates for lowest difference
  candidates <- test_shapes[test_shapes$area_diff <= benchmark, ]
  candidates$complexity <- NA       # best shape overall
  candidates_fits <- test_shapes     # best shape that fits completely in shape
  candidates_fits$complexity <- NA
  loop_indices <- candidates$index
  loop_indices_fits <- candidates_fits$index

  for (j in loop_indices_fits) {
    current_index <- as.character(j)
    if (current_index %in% rownames(candidates_fits)) {

      current_shape <- generate_matrix(candidates_fits[current_index, ]$width,
                                       candidates_fits[current_index, ]$height,
                                       candidates_fits[current_index, ]$max_grid,
                                       padding = "specified",
                                       candidates_fits[current_index, ]$top,
                                       candidates_fits[current_index, ]$bottom,
                                       candidates_fits[current_index, ]$left,
                                       candidates_fits[current_index, ]$right)

      # if it fits inside, record differences
      # as different method of calculating complexity
      if (fits_within(current_shape, reduced)) {
        candidates_fits[current_index, ]$complexity <- count_differences(
          current_shape, reduced)
      } else {
        # set a value much higher than possible
        candidates_fits[current_index, ]$complexity <- sum(reduced)^2
      }

      # reduce the list based on best recorded complexity candidate so far
      if (candidates_fits[current_index, ]$complexity < benchmark_fits) {
        benchmark <- candidates_fits[current_index, ]$complexity
        candidates_fits <-
          candidates_fits[candidates_fits$area_diff <= benchmark, ]
      }
    }
  }

  # find best options in candidates list
  for (k in loop_indices) {
    current_index <- as.character(k)
    if (current_index %in% rownames(candidates)) {

      current_shape <- generate_matrix(candidates[current_index, ]$width,
                                       candidates[current_index, ]$height,
                                       candidates[current_index, ]$max_grid,
                                       padding = "specified",
                                       candidates[current_index, ]$top,
                                       candidates[current_index, ]$bottom,
                                       candidates[current_index, ]$left,
                                       candidates[current_index, ]$right)

      # record current differences
      candidates[current_index, ]$complexity <- count_differences(
        current_shape, reduced)

      # reduce the list based on best recorded complexity candidate so far
      if (candidates[current_index, ]$complexity < benchmark) {
        benchmark <- candidates[current_index, ]$complexity
        candidates <- candidates[candidates$area_diff <= benchmark, ]
      }
    }
  }

  # reduce candidates
  candidates <-
    candidates[
      candidates$complexity == min(candidates$complexity), ]
  candidates_fits <-
    candidates_fits[
      candidates_fits$complexity == min(candidates_fits$complexity), ]

  # merge candidates
  candidates$fits <- FALSE
  candidates_fits$fits <- TRUE
  candidates_fits$fits[
    candidates_fits$complexity == (sum(reduced)^2)] <- FALSE

  if (nrow(candidates) > 0
      & nrow(candidates_fits) > 0) {
    candidates <- candidates[
      !(candidates$index %in% candidates_fits$index), ]
  }

  merged <- rbind(candidates, candidates_fits)

  return(merged)
}

