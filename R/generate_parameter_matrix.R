# GOAL: based on exact specifications, produce a possible result
  # if unspecified, allow to vary

# FIX:
  # limit size more, it can hang (or figure out why hanging in change N boxes)

# BUG:

# from find eligible vecs; this makes generation hang
# Error in if (side == "left" & current_vec[1] == 1) { :
#     argument is of length zero

# Error in as.vector(x, mode) :
# cannot coerce type 'closure' to vector of type 'any'
# not sure from where

# change N boxes takes a very long time
  # limit changes more or something?

generate_parameter_matrix <- function(area = 0, perimeter = 0,
                                      complexity = 0, complexity_fits = 0,
                                      grid_size = 10,
                                      sides = c(),
                                      iterations = 1000)
{
  # ADD
    # method to specify the baseline starting shape and max changes
    # like I want all 6x4 grids with few changes

  # limit grid sizes for now
  if (grid_size > 20) {
    return(print("PERIMETER: grid sizes larger than 20 not yet supported"))
  }

  # if number of sides to change unspecified, randomly determine how many
  all_sides <- c("left", "right", "top", "bottom")
  if (length(sides) == 0) {
    number_of_sides <- sample(1:4, 1)
    sides <- sample(all_sides, number_of_sides, replace = FALSE)
  }

  # randomly select area if unspecified
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

  if (complexity_fits == 0) {
    random_complexity_fits <- TRUE
  } else {
    random_complexity_fits <- FALSE
  }

  # start with a square

  finished <- FALSE
  # LOOP until we match: area, perimeter, complexity
  #iterations <- 100


  while (!finished & (iterations > 0)) {

    # potential parameters
    area_passed <- FALSE
    perimeter_passed <- FALSE
    complexity_passed <- FALSE
    complexity_fits_passed <- FALSE

    # 1 make N changes to the baseline
    number_of_changes <- sample(1:(floor((grid_size^2)*0.2)), 1)

    # re-randomize sides
    number_of_sides <- sample(1:4, 1)
    sides <- sample(all_sides, number_of_sides, replace = FALSE)

    # BUG HERE, want it to only sample sometimes
    if (random_area == TRUE) {
      area <- sample(area_bounds, 1)
    }

    # MIGHT NEED TO SPEED THIS UP
    # size of the basis for generate controlled matrix
    baseline_size <- sample(c(floor(sqrt(area)) - 1, floor(sqrt(area))), 1)

    print(paste0("Changes: ", number_of_changes))
    print(paste0("Area: ", area))

    test_matrix <- generate_controlled_matrix(
      number_of_changes,
      width = baseline_size,
      height = baseline_size,
      grid_size = grid_size,
      sides = sides)

    #compute_complexity(test_matrix)


    if (!is_centered(test_matrix)) {
      saved_m <- test_matrix
      test_matrix <- recenter_matrix(test_matrix)
    }

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

    test_area <- sum(test_matrix)
    test_perimeter <- calculate_perimeter(test_matrix)

    c <- compute_complexity(test_matrix)
    test_complexity <- min(c$complexity)

    if (TRUE %in% c$fits) {
      test_complexity_fits <- min(c$complexity[c$fits == TRUE])
    } else {
      test_complexity_fits <- -100
    }


    # 2 test values that need to be tested (or let them be random)
      # for testing purposes, print area, perimeter, complexity, sides

    if (random_area == TRUE
        | area == test_area) {
      area_passed <- TRUE
    }

    if (random_perimeter == TRUE
        | perimeter == test_perimeter) {
      perimeter_passed <- TRUE
    }

    if (random_complexity == TRUE
        | complexity == test_complexity) {
      complexity_passed <- TRUE
    }

    if (random_complexity_fits == TRUE
        | complexity == test_complexity_fits) {
      complexity_fits_passed <- TRUE
    }

    if (area_passed
        & perimeter_passed
        & complexity_passed
        & complexity_fits_passed) {
      finished <- TRUE
    }

    print(finished)
    # 3 if not matched, re-run with iteration limits
    iterations <- iterations - 1
    print(paste0("Iterations: ", iterations))
  }

# to do:
  # 1 fix bugs
    #	1) Similar area in compute complexity can give error
    # 2) Fix eligible vecs can give error too
    # 3) Iteration hangs at 1 sometimes but not sure why
    # 4) sometimes recenter makes the total area smaller, not sure why
  # 2 test centered
  # 3 re-center (or return fail if unable to)

  return(test_matrix)
}




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
