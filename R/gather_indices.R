# NOTE: this function is surprisingly slow, not sure if I can improve that
# from a matrix, move through matrix to gather every index in the "shape"
# to collect the size of the shape
gather_indices <- function(matrix)
{
  # list of all indices in the matrix
  index_list <- as.data.frame(which(matrix == 1, arr.ind = TRUE))

  # pick a random/first index with a 1
  start_x <- index_list[1,1]
  start_y <- index_list[1,2]  # could change these to random

  # add starting point to index list
  #indices <- data.frame(row = start_x, col = start_y)
  indices <- data.frame(matrix(ncol = 2, nrow = 0))
  current_positions <- data.frame(row = start_x,
                                  col = start_y)
  # save starting point to list of indices
  indices <- rbind(indices, current_positions)
  finished_gathering <- FALSE

  # method:
    # current positions begins with one starting point
    # check if current positions are in indices yet
    # if not, add them to indices
    # find all the neighbours of current positions as potential new neighbours
    # remove any potential new that are already in indices
      # otherwise this will repeat forever

  indices <- data.frame(matrix(ncol = 2, nrow = 0))
  starting_position <- data.frame(row = start_x,
                                  col = start_y)
  indices <- rbind(indices, starting_position)
  next_positions <- starting_position


  # LOOP
  while (!finished_gathering) {

    if (nrow(next_positions) == 0) {
      finished_gathering <- TRUE
    } else {
      # make the first entry of next positions the current position
      current_position <- next_positions[1, ]
      # and remove that entry from the list
      next_positions <- next_positions[-c(1), ]
    }

    # find all neighbours of current position
    current_neighbours <- locate_neighbours(matrix,
                                            current_position[1, 1],  # x
                                            current_position[1, 2]   # y
                                            )

    if (nrow(current_neighbours) > 0) {
      # for each neighbour
      for (i in 1:nrow(current_neighbours)) {
        # check if cell has a 1
        if ((matrix[current_neighbours[i, 1], current_neighbours[i, 2]]) == 1) {
          # tests if current position is already in indices
          current_n <- c(current_neighbours[i, 1],
                         current_neighbours[i, 2])
          already_found <- FALSE
          already_found <- sapply(
            X = paste0(current_n[1], current_n[2]),
            FUN = function(x) !is.na(match(x = x,
                                           table = paste0(indices[, 1],
                                                          indices[, 2])))
          )
          if (!already_found) {
            # add current position to indices
            indices <- rbind(indices,
                             c(current_n[1], current_n[2]))
            next_positions <- rbind(next_positions,
                                    c(current_n[1], current_n[2]))
          }
        }
      }
    }

  }
  return(indices)
}





# # if this position is a 1 and not found
# if ( (matrix[current_neighbours[1, i], current_neighbours[2, i]]) == 1
#      & already_found == FALSE) {
#   indices <- rbind(indices, current_neighbours)
# }


# if (nrow(new_neighbours) == 0) {
#   finished_gathering <- TRUE
# }
# print(paste0(
#   "Found cell ",
#   nrow(indices),
#   " of ",
#   sum(matrix)
# ))

  # OLD
  # while (!finished_gathering) {
  #
  #   # re-empty the potential neighbours
  #   new_neighbours <- data.frame(matrix(ncol = 2, nrow = 0))
  #   potential_new_neighbours <- data.frame(matrix(ncol = 2, nrow = 0))
  #
  #   for (i in 1:nrow(current_positions)) {
  #     # locate neighbours for each position
  #     current_neighbours <- locate_neighbours(matrix,
  #                                             current_positions[i, 1],  # x
  #                                             current_positions[i, 2],  # y
  #                                             nrow(matrix), ncol(matrix))
  #
  #     # rbind them to potential_new_positions
  #     colnames(current_neighbours) <- colnames(potential_new_neighbours)
  #     potential_new_neighbours <- rbind(
  #       potential_new_neighbours, current_neighbours
  #     )
  #   }
  #
  #   # add only potential new neighbours that aren't in indices already
  #   if (nrow(potential_new_neighbours) > 0) {
  #     for (j in 1:nrow(potential_new_neighbours)) {
  #       current_n <- c(potential_new_neighbours[j, 1],
  #                      potential_new_neighbours[j, 2])
  #       # tests if current position is already in indices
  #       already_found <- FALSE
  #       already_found <- sapply(
  #         X = paste0(current_n[1], current_n[2]),
  #         FUN = function(x) !is.na(match(x = x,
  #                                        table = paste0(indices[, 1],
  #                                                       indices[, 2])))
  #       )
  #       if (!already_found) {
  #         # add current position to indices
  #         new_neighbours <- rbind(new_neighbours,
  #                                 c(current_n[1], current_n[2]))
  #       }
  #     }
  #   }
  #
  #   colnames(new_neighbours) <- colnames(indices)
  #   indices <- rbind(indices, new_neighbours)
  #   # also remove duplicates
  #   indices <- indices[!duplicated(indices), ]
  #
  #
  #   # if none of the potential neighbours are not already in indices
  #   if (nrow(new_neighbours) == 0) {
  #     finished_gathering <- TRUE
  #   }
  #
  #   current_positions <- new_neighbours
  # }







# for each neighbour, test if already found
# for (i in 1:nrow(current_neighbours)) {
#   current_position <- c(current_neighbours[i, 1], current_neighbours[i, 2])
#
#   # tests if current position is already in indices
#   already_found <- sapply(
#     X = paste0(current_position[1], current_position[2]),
#     FUN = function(x) !is.na(match(x = x,
#                                    table = paste0(indices$row,
#                                                   indices$col)))
#   )
#
#   if (already_found) {
#     remove_neighbours <- c(remove_neighbours, i)
#   }
# }

# remove already found rows
# if (is.null(remove_neighbours) == FALSE) {
#   current_neighbours <- current_neighbours[-remove_neighbours, ]
# }

# print("current neighbours:")
# print(current_neighbours)


#positions <- found_indices
# while (nrow(positions) > 1) {
#
#   # then test multiple points
#   for (i in 1:nrow(positions)) {
#
#     # find neighbours with a 1
#     current_neighbours <- locate_neighbours(matrix,
#                                             positions[1], positions[2],
#                                             nrow(matrix), ncol(matrix))
#     colnames(current_neighbours) <- c("row", "col")
#   }
# }





# start at a position

# find all neighbours

# check if any are already found

# remove already found from that list

# add new unfound to the list

# re-run from the unfound
