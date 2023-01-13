
sub_box <- function(matrix) {

  eligible_indices <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(eligible_indices) <- c("i", "j")

  # some vectors to use later
  lr <- c("left", "right")
  tb <- c("top", "bottom")
  lb <- c("left", "bottom")
  rb <- c("right", "bottom")
  lt <- c("left", "top")
  rt <- c("right", "top")

  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (matrix[i, j] == 1){
        num_of_neighbours <- count_neighbours(matrix, i, j)
        eligible_cell <- TRUE  # set as true, several ways to be false below

        # if this cell has 1 neighbour, use
        if (num_of_neighbours == 1) {
          eligible_cell <- TRUE
        # if it has 4 neighbours, can't use
        } else if (num_of_neighbours == 4) {
          eligible_cell <- FALSE
        # if 2 or 3, then check each of the current cell's neighbours
        } else if (num_of_neighbours == 2 | num_of_neighbours == 3) {

          cell_neighbours <- find_edges(matrix, i, j)

          # if cell has 2 opposite neighbours, ineligible
          if (identical(sort(cell_neighbours), sort(lr))
              | identical(sort(cell_neighbours), sort(tb))) {
                eligible_cell <- FALSE
          } else {
            # if any neighbours have only opposite neighbours, ineligible
            if ("top" %in% cell_neighbours) {
              top_n <- count_neighbours(matrix, i-1, j)
              if (top_n == 1) {
                eligible_cell <- FALSE
              } else if (top_n == 2 | top_n == 3) {
                top_neighbours <- find_edges(matrix, i-1, j)
                if (identical(sort(top_neighbours), sort(lr))
                    | identical(sort(top_neighbours), sort(tb))) {
                  eligible_cell <- FALSE
                }
              }
            }
            if ("bottom" %in% cell_neighbours) {
              bottom_n <- count_neighbours(matrix, i+1, j)
              if (bottom_n == 1) {
                eligible_cell <- FALSE
              } else if (bottom_n == 2 | bottom_n == 3) {
                bottom_neighbours <- find_edges(matrix, i+1, j)
                if (identical(sort(bottom_neighbours), sort(lr))
                    | identical(sort(bottom_neighbours), sort(tb))){
                  eligible_cell <- FALSE
                }
              }
            }
            if ("left" %in% cell_neighbours) {
              left_n <- count_neighbours(matrix, i, j-1)
              if (left_n == 1) {
                eligible_cell <- FALSE
              } else if (left_n == 2 | left_n == 3) {
                left_neighbours <- find_edges(matrix, i, j-1)
                if (identical(sort(left_neighbours), sort(lr))
                    | identical(sort(left_neighbours), sort(tb))) {
                  eligible_cell <- FALSE
                }
              }
            }
            if ("right" %in% cell_neighbours) {
              right_n <- count_neighbours(matrix, i, j+1)
              if (right_n == 1) {
                eligible_cell <- FALSE
              } else if (right_n == 2 | right_n == 3) {
                right_neighbours <- find_edges(matrix, i, j+1)
                if (identical(sort(right_neighbours), sort(lr))
                    | identical(sort(right_neighbours), sort(tb))) {
                  eligible_cell <- FALSE
                }
              }
            }
            # if cell forms part of internal corner
            # then ineligible (technically could be but rare occurrence)
            if (lb[1] %in% cell_neighbours & lb[2] %in% cell_neighbours) {
              if (matrix[i+1, j-1] == 0) {
                eligible_cell <- FALSE
              }
            }
            if (rb[1] %in% cell_neighbours & rb[2] %in% cell_neighbours) {
              if (matrix[i+1, j+1] == 0) {
                eligible_cell <- FALSE
              }
            }
            if (lt[1] %in% cell_neighbours & lt[2] %in% cell_neighbours) {
              if (matrix[i-1, j-1] == 0) {
                eligible_cell <- FALSE
              }
            }
            if (rt[1] %in% cell_neighbours & rt[2] %in% cell_neighbours) {
              if (matrix[i-1, j+1] == 0) {
                eligible_cell <- FALSE
              }
            }
          }
        }

        if (eligible_cell) {
          eligible_indices <- rbind(eligible_indices,
                                    data.frame(i = i,  j = j))
        }
      }
    }
  }

  random_sub <- eligible_indices[sample(1:nrow(eligible_indices), 1), ]
  matrix[random_sub$i, random_sub$j] <- 0

  return(matrix)
}

# can use find edges to test if it has opposite side neighbours




# if it's a 1, then count neighbours
# if it has only 1 neighbour, good
# else if it has 4 neighbours, no
# else if it has 3 or 2 neighbours, then need to test it's neighbours
# find edges of current i/j cell
# count neighbours for each edge (find edge is essentially find neighbours)
# if "top" in neighbours, move to i-1, count those neighbours
# etc



# maybe need a new function to return neighbours, not count them
# or just check left/right/above/below with testing not out of bounds?
# wouldn't find edges tell you which way to look? it would
