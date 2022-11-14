
# dplyr dependency here might be removable once I rewrite this function

# element 1 is top left
# element 2 to the right of that, finish row

# order of points: bottom left, bottom right, top right, top left
# NOTE: drawing on a plot in quadrant 1 is flipped from R console matrix
# flipped around the x-axis
# ex:
# (0,0), (1,0), (1,1), (0,1) is instead
# (0, -1), (1, -1), (1, 0), (0, 0)

compute_plot_points <- function(matrix, gridline_size = 1)
{
  # starting parameters
  shape <- "square"
  element <- 1
  box_colour <- "blue"
  background_colour <- "white"
  size <- 1
  x_increment <- size * gridline_size
  y_increment <- size * gridline_size

  if (shape == "square") {
    num_of_coords <- 4
  }

  grid <- data.frame(matrix(nrow = 0, ncol = num_of_coords))
  colnames(grid) <- c("element", "shape", "x", "y")
  grid$shape <- as.character(grid$shape)

  # set initial y coord
  y_start <- 0

  # 1,1 in matrix is top left, 1,1 in plot is bottom left
    # so we flip it but plot it to look the same as the matrix
    # that way, if we want to draw lines on the plot, the locations are sensible

  for (i in 1:(nrow(matrix))) {

    # reset initial x coord
    x_start <- 0

    for (j in 1:ncol(matrix)) {

      # determine colour of cell
      if (matrix[i, j] == 1) {
        cell_colour <- box_colour
      } else {
        cell_colour <- background_colour
      }

      # create one cell
      grid_cell <- data.frame(
        element = c(rep(element, num_of_coords)),
        shape = c(rep(shape, num_of_coords)),
        x = c(x_start,
              x_start+size,
              x_start+size,
              x_start),
        y = c(y_start-size,
              y_start-size,
              y_start,
              y_start),
        draw_colour = c(rep(cell_colour, num_of_coords))
      )

      grid <- dplyr::bind_rows(grid, grid_cell)

      element <- element + 1

      # increment x to move grid cell right
      x_start <- x_start + x_increment

    }  # end m loop

    # decrement y to move grid cell down
    y_start <- y_start - y_increment

  }  # end n loop

  return(grid)
}
