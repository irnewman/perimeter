
# trying simplified method

add_box <- function(matrix) {

  eligible_indices <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(eligible_indices) <- c("i", "j")

  # some vectors to use later
  lr <- c("left", "right")
  tb <- c("top", "bottom")
  lb <- c("left", "bottom")
  rb <- c("right", "bottom")
  lt <- c("left", "top")
  rt <- c("right", "top")

  # if any cell is 0, and has a neighbour, add it
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (matrix[i, j] == 0){

        num_of_neighbours <- count_neighbours(matrix, i, j)
        eligible_cell <- TRUE
        cell_neighbours <- find_edges(matrix, i, j)

        # THIS ISN'T EXHAUSTIVE, still bug
          # maybe just check if any corners now have 4 neighbours?
            # if so, ineligible, that should work I think
        # but would that new 1 have 0's in a corner with a 1 on the diagonal?
        # if so, ineligible
        if (num_of_neighbours > 0) {

          # if a neighbour already has 3 neighbours, turning current cell to 1
            # makes a hole
          if (!("top" %in% cell_neighbours) & i > 1) {
            top_n <- count_neighbours(matrix, i-1, j)
            if (top_n >= 3) {
              eligible_cell <- FALSE
            }
          }
          if (!("bottom" %in% cell_neighbours) & i < nrow(matrix)) {
            bottom_n <- count_neighbours(matrix, i+1, j)
            if (bottom_n >= 3) {
              eligible_cell <- FALSE
            }
          }
          if (!("left" %in% cell_neighbours) & j > 1) {
            left_n <- count_neighbours(matrix, i, j-1)
            if (left_n >= 3) {
              eligible_cell <- FALSE
            }
          }
          if (!("right" %in% cell_neighbours) & j < ncol(matrix)) {
            right_n <- count_neighbours(matrix, i, j+1)
            if (right_n >= 3) {
              eligible_cell <- FALSE
            }
          }


          # if left and below both 0 but diagonally left/below is 1, ineligible
          if (!(lb[1] %in% cell_neighbours)
              & !(lb[2] %in% cell_neighbours)
              & i < nrow(matrix)
              & j > 1) {
            if (matrix[i+1, j-1] == 1) {
              eligible_cell <- FALSE
            }
          }
          if (!(rb[1] %in% cell_neighbours)
              & !(rb[2] %in% cell_neighbours)
              & i < nrow(matrix)
              & j < ncol(matrix)) {
            if (matrix[i+1, j+1] == 1) {
              eligible_cell <- FALSE
            }
          }
          if (!(lt[1] %in% cell_neighbours)
              & !(lt[2] %in% cell_neighbours)
              & i > 1
              & j > 1) {
            if (matrix[i-1, j-1] == 1) {
              eligible_cell <- FALSE
            }
          }
          if (!(rt[1] %in% cell_neighbours)
              & !(rt[2] %in% cell_neighbours)
              & i > 1
              & j < ncol(matrix)) {
            if (matrix[i-1, j+1] == 1) {
              eligible_cell <- FALSE
            }
          }
        }

        if ((count_neighbours(matrix, i, j) > 0) & eligible_cell) {
          eligible_indices <- rbind(eligible_indices, data.frame(i = i,  j = j))
        }
      }
    }
  }


  random_add <- eligible_indices[sample(1:nrow(eligible_indices), 1), ]
  matrix[random_add$i, random_add$j] <- 1


  # FIX NEXT: fixed I think
    # test that adding doesn't create a hole instead of just looking for it
    # does the new box create a diagonal corner?
  # if hole created, resample I guess
  # look for 0's with 4 neighbours
  # for (p in 1:nrow(matrix)) {
  #   for (q in 1:ncol(matrix)) {
  #     if (matrix[p, q] == 0) {
  #       current_cell_n <- count_neighbours(matrix, p, q)
  #       if (current_cell_n == 4) {
  #         failed_matrix <- TRUE
  #         print("found a gap")
  #       }
  #     }
  #   }
  # }



  return(matrix)
}
