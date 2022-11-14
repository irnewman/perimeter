
# NOTE: tidy this and other old functions up
# this is now how to specify the parameters for a given shape
  # generate controlled needs to be finished
# NOTE: need to make compatible with new padding function too


# generate random or given vector as matrix
  # also want another to create all given matrices
  # could I give a vector of lengths and it plots them?
    # like c(1, 3, 1, 4, 5)

# TO DO:
  # make it so that a row doesn't always have to start with 1
  # but then also test that the grid doesn't have gaps
    # not sure how to test that, compare each pair of rows
    # then check that at least one index has 1,1


# RENAME PROBABLY: create_plot_area or something similar
  # this function creates a boolean matrix of which boxes will be plotted
  # another function changes that to coordinates to draw them
  # can use this matrix to calculate perimeter


# currently, this generates it randomly
  # I could also make it generate every possible combination

# default values when arguments are not supplied
#v <- c(0, 0, 5, 5, 5, 5, 5, 0, 0)  # height from bottom

# check if matrix is large enough
# if ( (n<3) | (m<3) ) {
#   print("PERIMETER: n and m must be at least 3 (a 3x3 grid or larger)")
# }

# # check if any row is longer than row length
# if (length(v[v > m]) > 0) {
#   print("PERIMETER: at least one vector value larger than grid bounds")
# }

# going to turn this into a function
#grid_matrix <- data.frame(matrix(nrow = n, ncol = n))



create_matrix <- function(width, height)
{
  matrix <- data.frame(matrix(nrow = height, ncol = width))
  matrix[1:nrow(matrix), 1:ncol(matrix)] <- 1
  return(matrix)
}





# OLD FUNCTION
# create_matrix <- function(n = 6, area = 36, p = 2,
#                           v = c(), random = TRUE,
#                           random_type = "full")
# {
#
#   # check if area and supplied vector are compatible
#   if ( (random == FALSE) & (area != sum(v)) ) {
#     print("PERIMETER: specified vector and area are not equal")
#   }
#
#   grid_size <- n + (p*2)
#   # FIX ARGUMENTS FOR THESE
#   # given vector
#   if (random == FALSE) {
#     grid_matrix <- generate_fixed_matrix(v, p)
#   }
#
#   if ( (random == TRUE) & random_type == "full" ) {
#     grid_matrix <- generate_random_matrix(area, p, grid_size)
#   }
#
#   # if ( (random == TRUE) & random_type == "controlled") {
#   #   grid_matrix <- generate_controlled_matrix()  # UNFINISHED FUNCTION
#   # }
#
#   return(grid_matrix)
# }







 # # SIMPLE ITEMS
  # # random
  # if (random == TRUE) {
  #   boxes <- area
  #   for (i in 1:nrow(grid_matrix)) {
  #     row_boxes <- sample(1:m, 1)
  #     boxes <- boxes - row_boxes
  #     if (boxes > 0) {
  #       grid_matrix[i, ] <- c(rep(1, row_boxes), rep(0, m - row_boxes))
  #     } else {
  #       grid_matrix[i, ] <- c(rep(0, m))
  #     }
  #   }
  # }


  # # COMPLEX: UNFINISHED BUT ON THE RIGHT TRACK
  # # random
  # if (random == TRUE) {
  #   boxes <- area
  #   for (i in 1:nrow(grid_matrix)) {
  #     row_boxes <- sample(1:m, 1)
  #     boxes <- boxes - row_boxes
  #     if (boxes > 0) {
  #       row_box_index <- sample(m - row_boxes, 1)
  #       grid_matrix[i, ] <- c(rep(0, row_box_index),
  #                             rep(1, row_boxes),
  #                             rep(0, m - (row_boxes+row_box_index)))
  #     } else {
  #       grid_matrix[i, ] <- c(rep(0, m))
  #     }
  #
  #     test_grid <- t(grid_matrix)
  #
  #     for (k in 1:(ncol(test_grid) - 1)) {
  #       r1 <- match(x[,k], 1)
  #       r2 <- match(x[,k+1], 1)
  #     }
  #
  #
  #     check_split <- r1 == r2
  #
  #     if (TRUE %in% check_split)
  #
  #
  #
  #
  #
  #
  #   }
  # }







  # HERE: we have just boxes in a matrix
  # IDEALLY: we can make, from this grid, another set that draws each shape




  #return(grid_matrix)


