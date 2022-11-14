
# this is to generate a floating square within the bounds of the total area

# add errors for padding too big or not compatible

generate_matrix <- function(width = 6, height = 6, grid_size = 10,
                            padding = "generated",
                            top = 0, bottom = 0, left = 0, right = 0)
{
  # return if parameters incompatible
  if (padding == "specified"
      & (((width + left + right) > grid_size)
        | ((height + top + bottom) > grid_size))
  ) {
    print("PERIMETER: incompatible parameters specified")
    print("width + left + right and height + top + bottom must equal grid size")
    return()
  }

  # create the shape without the padding
  shape <- create_matrix(width, height)

  # use arguments for padding
  if (padding == "specified") {
    p <- list(top = top,
              bottom = bottom,
              left = left,
              right = right)
  }

  # determine amount of space on each side
  if (padding == "generated") {
    p <- determine_padding(width, height, grid_size,
                           top, bottom, left, right)
  }


  return(increase_matrix(shape,
                         top = p$top,
                         bottom = p$bottom,
                         left = p$left,
                         right = p$right))
}




# NOTE: plotting reversed because plot matrix bugged


# fill the grid (should work for square or rect)
#return(fill_matrix(grid_size, padding))


# for (i in 1:nrow(grid_matrix)) {  # i = vertically, top to bottom
#   if ( (i <= padding$top) | (i > grid_size - padding$bottom)) {
#     grid_matrix[i, ] <- c(rep(0, grid_size))
#   } else {
#     grid_matrix[i, ] <- c(
#       rep(0, padding$left),
#       rep(1, (grid_size - (padding$left+padding$right))),
#       rep(0, padding$right))
#   }
# }
