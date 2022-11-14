


# matrix, indices list, current positions = start, new positions = 1

# while new positions isn't empty
  # add current positions to indices
  # find new positions as all neighbours of current positions not in indices
  # current positions = new positions
  # end while





# DELETE AFTER







#check_indices(matrix, start_x, start_y)









# rename
check_indices <- function(matrix, x_pos, y_pos)
{

  # tests if current position is already in indices
  already_found <- sapply(
    X = paste0(x_pos, y_pos),
    FUN = function(x) !is.na(match(x = x,
                                   table = paste0(indices[, 1],
                                                  indices[, 2])))
  )

  if (!already_found) {
    # add current position to indices
    indices <<- rbind(indices, c(x_pos, y_pos))
  }



  # find neighbours with a 1
  print("position")
  print(x_pos)
  print(y_pos)
  current_neighbours <- locate_neighbours(matrix,
                                          x_pos, y_pos)
  colnames(current_neighbours) <- c("row", "col")
  #remove_neighbours <- c()

  print("current neighbours:")
  print(current_neighbours)



  if (nrow(current_neighbours) > 0) {

    for (i in 1:nrow(current_neighbours)) {

      current_position <- c(current_neighbours$row[i],
                            current_neighbours$col[i])
      print("current_position")
      print(current_position)

      already_done <- sapply(
        X = paste0(current_position[1], current_position[2]),
        FUN = function(x) !is.na(match(x = x,
                                       table = paste0(indices[, 1],
                                                      indices[, 2])))
      )

      if (!already_done) {
        indices <<- rbind(indices, current_position)
        gather_indices(matrix, current_position[1], current_position[2])
      }
      # add neighbours to indices


      #print("indices:")
      #print(indices)

      # remove duplicates
      #indices <<- indices[!duplicated(indices), ]


    }
  }
}
