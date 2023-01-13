
# needs to be rewritten to match upcoming changes to change N boxes
generate_controlled_matrix <- function(changes,
                                       width = 6, height = 6, grid_size = 10,
                                       sides = c(),
                                       v_offset = "top", h_offset = "left",
                                       baseline = "square",
                                       operation = "both")
{

  # shape <- generate_matrix(width, height, grid_size)
  #
  # # if number of sides to change unspecified, randomly determine how many
  # all_sides <- c("left", "right", "top", "bottom")
  # if (length(sides) == 0) {
  #   number_of_sides <- sample(1:4, 1)
  #   sides <- sample(all_sides, number_of_sides, replace = FALSE)
  # }
  #
  # # matrix <- data.frame(matrix(nrow = grid_size, ncol = grid_size))
  # # padding <- determine_padding(width, height, grid_size,
  # #                              v_offset, h_offset)
  #
  #
  #
  # # changes = number of additions/subtractions to the baseline
  #
  # # baseline could be rectangle tall or long, too
  #   # so then prefer to have an m argument too?
  #
  #
  # # 1 if size of b not equal to gridsize, error
  #
  #
  #
  #
  #
  #
  #
  # return(change_n_boxes(shape, changes, operation, sides))

  return()
}











  # 2 randomly determine how many additions and how many subtractions
  # num_of_additions <- sample(0:changes, 1)
  # num_of_subtractions <- changes - num_of_additions
  #
  # # 3 when add/sub, sample which side to change
  # # then add/sub to baseline b, repeatedly
  # for (i in 1:num_of_additions) {
  #   b <- add_n_boxes(b, sample(sides_to_change, 1))
  # }
  # for (i in 1:num_of_subtractions) {
  #   b <- sub_n_boxes(b, sample(sides_to_change, 1))
  # }

  # ADD N
    # find eligible rows in b, based on which side is to be added
    # pick a row to change
    # make the change
    # return the new matrix

  # CHECK IF NEW MATRIX IS N CHANGES FROM BASELINE
    # add/sub in a row could randomly counteract each other
    # so re-run if failed





# start with a basic square of set dimensions, eg 5x5
# total space should be at least 7x7 then, and basic square drawn in the middle
# can add/remove boxes to any side
# can compute variance from the basic square as complexity
# and how many sides vary
# and length of longest variance maybe


# randomness
# how many and which sides to alter
# how many alterations, and of what max length
# how many "removed" boxes from the square
# how much area different from the square


# parameters
# difference in area (could be 0)
# number of changes (could be just 1 block different)
# max change distance
# max number of sides to alter



# complexity is number of boxes different from the baseline
# 1. generate baseline
# 2. generate features to vary from the baseline (sides, area/perim, number..)
# 3. compute a new random matrix based on said features
# 4. calculate indices, and so forth (in another function actually)


# RANDOMNESS: make functions to do these, I guess (and some to decide which to do)
# 1. shift a column up/down
# 2. shift a row left/right
# 3. remove a block/blocks
# 4. add a block/blocks
# 5. make a fully randomized shape
# - could specify limits to variance from shape? not sure
