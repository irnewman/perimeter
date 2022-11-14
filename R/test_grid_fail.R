
# fails if more than one piece, returns TRUE if failed

# 1 - if there are any floating pieces, it failed (no neighbours)
# 2 - if there are any pieces that touch something but they form a smaller shape



test_grid_fail <- function(matrix, exhaustive = FALSE) {

  if ( (nrow(matrix) > 20) ) {
    print("PERIMETER: warning, grid sizes greater than 20x20 ")
    print("  will extend the time to create stimuli considerably, ")
    print("  not recommended for use at this time.")
  }
  if ( (nrow(matrix) > 20) & exhaustive == TRUE) {
    print("PERIMETER: warning, grid sizes greater than 20x20 ")
    print("  with exhaustive = TRUE may take several minutes to complete,")
    print("  not recommended for use at this time.")

  }




  failed_matrix <- FALSE

  # are there any singular floating cells
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (matrix[i, j] == 1) {
        current_cell <- count_neighbours(matrix, i, j)
        if (current_cell == 0) {
          failed_matrix <- TRUE
          #print("found one")
        }
      }
    }
  }

  # any positions in adjacent rows that don't connect
  for (k in 1:(nrow(matrix) - 1)) {
    current_row <- match(matrix[k, ], 1)  # is there a "1" in row i
    adjacent_row <- match(matrix[k+1, ], 1)  # and i+1

    check_split <- current_row == adjacent_row

    if ( (any(1 %in% current_row))
         & (any(1 %in% adjacent_row))
         & all(is.na(check_split))
    ) {  # if true then split, failed and remake
      failed_matrix <- TRUE
      #print(paste0(k, " failed"))
    }
  }

  # any positions in adjacent columns that don't connect
  for (l in 1:(ncol(matrix) - 1)) {
    current_col <- match(matrix[,l], 1)  # is there a "1" in column i
    adjacent_col <- match(matrix[,l+1], 1)  # and i+1
    # a fail would have 1's, but at no point do any of them touch

    # indices where 1's touch (means it isn't split into pieces)
    check_split <- current_col == adjacent_col

     # must return all true to fail the matrix

    # if current_row has a 1 AND adjacent row has a 1
    # AND checksplit is ALL NA
    # THEN FAILED
    if ( (any(1 %in% current_col))
         & (any(1 %in% adjacent_col))
         & all(is.na(check_split))
         ) {  # if true then split, failed and remake
      failed_matrix <- TRUE
      #print(paste0(k, " failed"))
    }
  }

  # look for 0's with 4 neighbours
  for (p in 1:nrow(matrix)) {
    for (q in 1:ncol(matrix)) {
      if (matrix[p, q] == 0) {
        current_cell_n <- count_neighbours(matrix, p, q)
        if (current_cell_n == 4) {
          failed_matrix <- TRUE
          #print("found a gap")
        }
      }
    }
  }



  # only do this for small enough matrices or it will be very slow
  if (nrow(matrix) <= 20
      | exhaustive == TRUE) {


    # gather the indices into the indices data frame
    indices <- gather_indices(matrix)

    if (nrow(indices) != sum(matrix)) {
      failed_matrix <- TRUE
      #print("we found a floating piece")
    }
  }




  return(failed_matrix)
}


