
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
  grid_cols <- c("element", "edge", "side", "shape",
                 "x", "y", "xend", "yend",
                 "cell_colour", "i", "j")
  sides <- c("bottom", "right", "top", "left")

  if (shape == "square") {
    num_of_coords <- 4
  }

  grid <- data.frame(matrix(nrow = 0, ncol = length(grid_cols)))
  colnames(grid) <- grid_cols
  grid$shape <- as.character(grid$shape)
  grid$cell_colour <- as.character(grid$cell_colour)

  # set initial y coord
  y_start <- 0

  # 1,1 in matrix is top left, 1,1 in plot is bottom left
    # so we flip it but plot it to look the same as the matrix
    # that way, if we want to draw lines on the plot, the locations are sensible

  # TO DRAW OUTLINE:
    # 1. function to return which cells have which borders
    # 2. draw line at each side, background colour for non-border
    # 3. save values to grid and plot them with plot_matrix

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

      if (matrix[i, j] == 1) {
        # nested: if neighbour on top, border colour on top background
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
        xend = c(x_start+size,
                 x_start+size,
                 x_start,
                 x_start),
        yend = c(y_start-size,
                 y_start,
                 y_start,
                 y_start-size),
        cell_colour = c(rep(cell_colour, num_of_coords)),
        i = i,
        j = j
      )

      grid <- dplyr::bind_rows(grid, grid_cell)

      element <- element + 1

      # increment x to move grid cell right
      x_start <- x_start + x_increment

    }  # end m loop

    # decrement y to move grid cell down
    y_start <- y_start - y_increment

  }  # end n loop

  grid$edge <- 1:nrow(grid)
  grid$side <- rep(sides, nrow(grid)/num_of_coords)

  return(grid)
}
