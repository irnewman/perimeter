# GOAL: based on exact specifications, produce a possible result
  # if unspecified, allow to vary




# Error in as.vector(x, mode) :
# cannot coerce type 'closure' to vector of type 'any'
# not sure from where

# change N boxes takes a very long time
  # limit changes more or something?


############
# rewrite
############
# BUG: when n sides = 1 and area diff is large, can hang infinitely

# 1. if area = 14, randomly pick a square or rectangle of similar area

# s = floor(sqrt(area))
# c(s-1, s, s+1, s+2) [use range parameter for bigger differences? probably enough]
# pick two randomly, make a rectangle (or square) of that size
# change N boxes from that to match area

  # so, a 3x4=12 or 4x3=12 or 4x4=16 or 3x3=9
  # make N changes to that baseline so that the area matches
  # so pick the 3 closest products above and below and randomly select 1
  # make N changes so that area matches
# 2. if perimeter = 0, passes automatically, otherwise, re-run until match
# 3. if complexity = 0, passes automatically, otherwise, re-run until match
  # 4. same for corners

generate_parameter_matrix <- function(area = 0, perimeter = 0,
                                      complexity = 0, corners = 0,
                                      grid_size = 6, max_difference = 10,
                                      iterations = 1000)
{

  # limit grid sizes for now
  if (grid_size > 20) {
    return(print("PERIMETER: grid sizes larger than 20 not yet supported"))
  }

  # randomly select parameters if each unspecified (could make function)
  if (area == 0) {
    area_bounds <- ((grid_size^2)/4):((grid_size^2)/2)
    area <- sample(area_bounds, 1)
    random_area <- TRUE
  } else {
    random_area <- FALSE
  }

  if (perimeter == 0) {
    random_perimeter <- TRUE
  } else {
    random_perimeter <- FALSE
  }

  if (complexity == 0) {
    random_complexity <- TRUE
  } else {
    random_complexity <- FALSE
  }

  if (corners == 0) {
    random_corners <- TRUE
  } else {
    random_corners <- FALSE
  }

  finished <- FALSE
  while (!finished & (iterations > 0)) {

    # potential parameters
    area_passed <- FALSE
    perimeter_passed <- FALSE
    complexity_passed <- FALSE
    corners_passed <- FALSE

    # randomly select baseline size
    s = floor(sqrt(area))
    product_range <- c(s-1, s, s+1, s+2)

    # generate a baseline rect with a random difference from the area
    area_diff <- max_difference + 1
    while (abs(area_diff) > max_difference) {
      p1 <- sample(product_range, 1)
      p2 <- sample(product_range, 1)
      area_diff <- area - (p1 * p2)  # negative outcome is allowed
    }

    # create baseline
    b <- generate_matrix(p1, p2, grid_size)

    # if diff = 0, no sides to change, else determine how many sides to change
      # eg area 16 and picked 4x4 square, then done
    if (area_diff == 0) {
      area_passed <- TRUE
    }

    while (!area_passed) {

      # create vector of changes that will sum to the area diff (add/sub)
      change_values <- sum_to_n(area_diff, 2, max_difference)

      # test that b +/- changes will not exceed bounds 1 to max area
      b_sum <- sum(b)
      for (i in change_values) {
        b_sum <- b_sum + i
        if (b_sum <= 1 | b_sum > grid_size^2) {
          print("changes exceeded the bounds, fix this bug")
          print(change_values)
        }
      }

      # rotate b random number of times
      b <- randomize_orientation(b)

      # make changes to b
      for (i in 1:2) {
        if (change_values[i] >= 0) {
          b <- add_n_boxes(b, abs(change_values[i]))
        } else {
          b <- sub_n_boxes(b, abs(change_values[i]))
        }
      }

      # test for area match
      if (sum(b) == area) {
        area_passed <- TRUE
      } else {
        # re-create baseline and start over
        b <- generate_matrix(p1, p2, grid_size)
      }
    }

    # compute indices and check for pass
    test_matrix <- b
    test_perimeter <- calculate_perimeter(test_matrix)
    c <- compute_complexity(test_matrix)
    test_complexity <- min(c$complexity)
    test_corners <- count_sides(test_matrix)

    if (random_perimeter == TRUE
        | perimeter == test_perimeter) {
      perimeter_passed <- TRUE
      # print(paste0("Perimeter match: ", test_perimeter))
    }

    if (random_complexity == TRUE
        | complexity == test_complexity) {
      complexity_passed <- TRUE
      # print(paste0("Complexity match: ", test_complexity))
    }

    if (random_corners == TRUE
        | corners == test_corners) {
      corners_passed <- TRUE
      # print(paste0("Corners match: ", test_corners))
    }

    if (!is_centered(test_matrix)) {
      saved_m <- test_matrix
      test_matrix <- recenter_matrix(test_matrix)
    }

    if (area_passed
        & perimeter_passed
        & complexity_passed
        & corners_passed
    ) {
      finished <- TRUE
    }

    # if not matched, re-run with iteration limits
    iterations <- iterations - 1
    print(paste0("Iterations: ", iterations))
    # print(sum(test_matrix))
  }


  return(test_matrix)
}




# if number of sides to change unspecified, randomly determine how many
# all_sides <- c("left", "right", "top", "bottom")
# if (length(sides) == 0) {
#   number_of_sides <- sample(1:4, 1)
#   sides <- sample(all_sides, number_of_sides, replace = FALSE)
# }

# if no parameters specified, full random
# not sure if I need this at all though
# full_random <- FALSE
# if (area == 0
#     & perimeter == 0
#     & complexity == 0
#     & corners == 0) {
#   full_random <- TRUE
# }





# else {
#   # select random number of sides to change
#   number_of_ops <- sample(1:2, 1)
#   # re-sample if selected more sides than changes to make
#   while (abs(area_diff) < number_of_sides) {
#     number_of_sides <- sample(1:4, 1)
#   }
# }















    # if change n boxes returned an error, start again
    # if (b == 0) {
    #   print("b was 0")
    #   next
    # }


    ############################################
    # MIGHT NEED TO SPEED THIS UP
    # size of the basis for generate controlled matrix
    #baseline_size <- sample(c(floor(sqrt(area)) - 1, floor(sqrt(area))), 1)

    # print(paste0("Changes: ", number_of_changes))
    # print(paste0("Area: ", area))

    # test_matrix <- generate_controlled_matrix(
    #   number_of_changes,
    #   width = baseline_size,
    #   height = baseline_size,
    #   grid_size = grid_size,
    #   sides = sides)



    #compute_complexity(test_matrix)




    #plot_matrix(test_matrix)

    # ADD BACK
    # matrix_string <- paste(as.vector(unlist(matrix)), collapse = "")



    # want number of flat sides too (need a function for that)
    # print(paste0("Baseline size: ", baseline_size))
    # print(paste0("Area: ", calculate_area(test_matrix)))
    # print(paste0("Perimeter: ", calculate_perimeter(test_matrix)))
    # print(paste0("Complexity: ", min(c$complexity)))
    # print(paste0("Sides: ", sides))
    # print(paste0("Changes: ", number_of_changes))
    # print(paste0("Centered: ", is_centered(test_matrix)))

    #test_area <- sum(test_matrix)


    # if (TRUE %in% c$fits) {
    #   test_complexity_fits <- min(c$complexity[c$fits == TRUE])
    # } else {
    #   test_complexity_fits <- -100
    # }


    # 2 test values that need to be tested (or let them be random)
      # for testing purposes, print area, perimeter, complexity, sides

    # if (random_area == TRUE
    #     | area == test_area) {
    #   area_passed <- TRUE
    #   print(paste0("Area matches: ", test_area))
    # }




# to do:
  # 1 fix bugs
    #	1) Similar area in compute complexity can give error
    # 2) Fix eligible vecs can give error too
    # 3) Iteration hangs at 1 sometimes but not sure why
    # 4) sometimes recenter makes the total area smaller, not sure why
  # 2 test centered
  # 3 re-center (or return fail if unable to)





  # if perimeter not 0, re-run until we find the right perimeter

  # 1 how to determine width/height?
  # shape <- generate_matrix(width, height, grid_size)

  # also need to return other info or do that in another function?


# baseline_shape <- generate_matrix(width = baseline_size,
#                                   height = baseline_size,
#                                   grid_size)

# if (method == "random") {
#   matrix <- generate_random_matrix(area, grid_size)
# }

# width = 6, height = 6, grid_size = 10,
# area = 0, perimeter = 0,
# complexity = 0,
# changes = 0, sides = c(), operations,
# baseline, method = "parameters"


# NOTE: leave random options for all of these
# 1 - a baseline shape
# 2 - the area of that baseline
# 3 - the desired area, perimeter, and complexity
# maybe - changes and sides
# possibly - randomly do changes/sides, and return how many were the result



# parameters
# changes - random or specified
# sides - random or specified
# type - random or specified
# area - specified (could allow varied)
# perimeter - specified (could allow varied)
# complexity - specified
# BECAUSE: if I want random area/perimeter, I can just iterate controlled ones


# test area of finished matrix
# test perimeter of finished matrix
# test complexity of finished matrix

# any failures = re-run, limit iterations


# return: the matrix (the number of changes, sides changed)
# could return a list with the matrix and other values all together
