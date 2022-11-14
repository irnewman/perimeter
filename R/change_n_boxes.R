
change_n_boxes <- function(matrix, number_of_boxes, sides = c(),
                           operation = "both", iterations = 10)
{

  area <- sum(matrix)
  area_criterion <- round(area * 0.3, digits = 0)

  if (number_of_boxes >= area_criterion) {
    # print("PERIMTER: warning, making changes larger than approximately 30% of")
    # print("  total area of the matrix shape will result in slow creation")
    # print("  or may not finish at all")
    print_iterations <- TRUE
    return(matrix)  # change later
  } else {
    print_iterations <- FALSE
  }

  additions <- 0
  subtractions <- 0

  # determine how many additions subtractions first
  if (operation == "addition") {
    additions <- number_of_boxes
  }
  if (operation == "subtraction") {
    subtractions <- number_of_boxes
  }
  if (operation == "both") {
    additions <- sample(0:number_of_boxes, 1)
    subtractions <- number_of_boxes - additions
  }

  # check if changes will fit
  empty_spaces <- ( nrow(matrix) * ncol(matrix) ) - sum(matrix)
  if (empty_spaces < additions) {
    return(print("PERIMETER: not enough empty spaces to add that many boxes"))
  }
  if (subtractions > sum(matrix)) {
    return(print("PERIMETER: not enough boxes to remove that many boxes"))
  }

  # randomly determine number of sides to alter
  all_sides <- c("left", "right", "top", "bottom")
  if (length(sides) == 0) {
    number_of_sides <- sample(1:4, 1)
    sides <- sample(all_sides, number_of_sides, replace = FALSE)
  }

  # make new matrix to change
  new_matrix <- matrix
  adds <- additions
  subs <- subtractions
  failed_matrix <- TRUE
  # iterations <- 1000
  max_iterations <- iterations

  while (failed_matrix & iterations > 0) {  # passes test

    if (print_iterations) {
      # print(paste0("Iteration ", (max_iterations - iterations)+1,
      #              " of ", max_iterations))
    }

    while (adds > 0) {
      current_sum <- sum(new_matrix)
      current_side <- sample(sides, 1)
      new_matrix <- change_box(new_matrix, current_side,
                               operation = "addition")

      # need this because it returns original matrix if no eligible vectors
      if (sum(new_matrix) > current_sum) {
        adds <- adds - 1
      }
    }

    while (subs > 0) {
      current_sum <- sum(new_matrix)
      current_side <- sample(sides, 1)
      new_matrix <- change_box(new_matrix, current_side,
                               operation = "subtraction")

      # need this because it returns original matrix if no eligible vectors
      if (sum(new_matrix) < current_sum) {
        subs <- subs - 1
      }
    }

    # if failed, reset matrix and counters
    if (test_grid_fail(new_matrix)
        | count_differences(matrix, new_matrix) != number_of_boxes) {
      new_matrix <- matrix
      adds <- additions
      subs <- subtractions
    } else {
      failed_matrix <- FALSE  # ends while loop
    }
    iterations <- iterations - 1
  }

  # if (method == "selective") {}  # re-add in future
  # if (method == "random") {}

  if (!failed_matrix) {
    return(new_matrix)
  } else {
    return()
  }

}

# TO DO: add a method argument, do it pure random as I have so far
# OR: a method that will run after a few failed attempts
# which will only select certain row/col indices to not overlap ever
# NOTE: this may make test-grid-fail obsolete? hopefully, that is a bottleneck
# NOTE: allow specification of which sides do which, like subtraction left/right


# NEW METHOD - currently bugged
# if (method == "selective") {
#   # list of indices not to re-use between subtraction and addition
#     # kinda tedious but works for now
#     # another function to do this doesn't seem to help much
#
#
#
#   # make all subtractions first
#   while (subs > 0) {
#
#     if ("left" %in% sides) {
#       s_left <- find_eligible_vecs(matrix, side = "left",
#                                    type = "subtraction")
#       a_left <- find_eligible_vecs(matrix, side = "left",
#                                    type = "addition")
#       sub_left <- sample(s_left, sample(1:(length(s_left)-1)))
#       add_left <- setdiff(a_left, sub_left)
#     }
#     if ("right" %in% sides) {
#       s_right <- find_eligible_vecs(matrix, side = "right",
#                                     type = "subtraction")
#       a_right <- find_eligible_vecs(matrix, side = "right",
#                                     type = "addition")
#       sub_right <- sample(s_right, sample(1:(length(s_right)-1)))
#       add_right <- setdiff(a_right, sub_right)
#     }
#     if ("top" %in% sides) {
#       s_top <- find_eligible_vecs(matrix, side = "top",
#                                   type = "subtraction")
#       a_top <- find_eligible_vecs(matrix, side = "top",
#                                   type = "addition")
#       sub_top <- sample(s_top, sample(1:(length(s_top)-1)))
#       add_top <- setdiff(a_top, sub_top)
#     }
#     if ("bottom" %in% sides) {
#       s_bottom <- find_eligible_vecs(matrix, side = "bottom",
#                                      type = "subtraction")
#       a_bottom <- find_eligible_vecs(matrix, side = "bottom",
#                                      type = "addition")
#       sub_bottom <- sample(s_bottom, sample(1:(length(s_bottom)-1)))
#       add_bottom <- setdiff(a_bottom, sub_bottom)
#     }
#
#
#     current_side <- sample(sides, 1)
#     if (current_side == "left") {
#       selected_index <- sample(sub_left, 1)
#     }
#     if (current_side == "right") {
#       selected_index <- sample(sub_right, 1)
#     }
#     if (current_side == "top") {
#       selected_index <- sample(sub_top, 1)
#     }
#     if (current_side == "bottom") {
#       selected_index <- sample(sub_bottom, 1)
#     }
#
#     print(new_matrix)
#     print("subs")
#     print(current_side)
#     print(selected_index)
#     new_matrix <- change_box(new_matrix, current_side, type = "subtraction",
#                              change_index = selected_index)
#     subs <- subs - 1
#   }
#
#   # then all additions
#   while (adds > 0) {
#
#     if ("left" %in% sides) {
#       s_left <- find_eligible_vecs(matrix, side = "left",
#                                    type = "subtraction")
#       a_left <- find_eligible_vecs(matrix, side = "left",
#                                    type = "addition")
#       sub_left <- sample(s_left, sample(1:(length(s_left)-1)))
#       add_left <- setdiff(a_left, sub_left)
#     }
#     if ("right" %in% sides) {
#       s_right <- find_eligible_vecs(matrix, side = "right",
#                                     type = "subtraction")
#       a_right <- find_eligible_vecs(matrix, side = "right",
#                                     type = "addition")
#       sub_right <- sample(s_right, sample(1:(length(s_right)-1)))
#       add_right <- setdiff(a_right, sub_right)
#     }
#     if ("top" %in% sides) {
#       s_top <- find_eligible_vecs(matrix, side = "top",
#                                   type = "subtraction")
#       a_top <- find_eligible_vecs(matrix, side = "top",
#                                   type = "addition")
#       sub_top <- sample(s_top, sample(1:(length(s_top)-1)))
#       add_top <- setdiff(a_top, sub_top)
#     }
#     if ("bottom" %in% sides) {
#       s_bottom <- find_eligible_vecs(matrix, side = "bottom",
#                                      type = "subtraction")
#       a_bottom <- find_eligible_vecs(matrix, side = "bottom",
#                                      type = "addition")
#       sub_bottom <- sample(s_bottom, sample(1:(length(s_bottom)-1)))
#       add_bottom <- setdiff(a_bottom, sub_bottom)
#     }
#
#
#     current_side <- sample(sides, 1)
#     if (current_side == "left") {
#       selected_index <- sample(add_left, 1)
#     }
#     if (current_side == "right") {
#       selected_index <- sample(add_right, 1)
#     }
#     if (current_side == "top") {
#       selected_index <- sample(add_top, 1)
#     }
#     if (current_side == "bottom") {
#       selected_index <- sample(add_bottom, 1)
#     }
#
#     print(new_matrix)
#     print("adds")
#     print(current_side)
#     print(selected_index)
#     new_matrix <- change_box(new_matrix, current_side, type = "addition",
#                              change_index = selected_index)
#     adds <- adds - 1
#   }
# }
  # keep sampling until it meets criteria
    #


    #print(current_side)
    #print(new_matrix)

    # if (current_side == "left") {
    #   new_matrix <- add_row_box(new_matrix, side = "left")
    # }
    # if (current_side == "right") {
    #   new_matrix <- add_row_box(new_matrix, side = "right")
    # }
    # if (current_side == "top") {
    #   new_matrix <- add_col_box(new_matrix, side = "top")
    # }
    # if (current_side == "bottom") {
    #   new_matrix <- add_col_box(new_matrix, side = "bottom")
    # }


    #print(new_matrix)




