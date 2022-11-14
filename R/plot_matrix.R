
# ISSUES:
# 1. only plots nicely with a square NxM right now
# 2. would like "mouth" shaped plots too, complicated to apply


# take a given matrix, create a plot of it, save the plot for output
  # 1. reshape the matrix (try to do this without reshape package)
  # 2. use ggplot and ggforce



# GRID LINES
# change the increment to get rid of them (variable, new function)
# for some reason, new grids are showing the lines on the background
  # I don't want that

#library(tidyverse)
#library(ggforce)

# anything larger than 100x100 will take several minutes per image


plot_matrix <- function(grid_matrix, gridline_size = 1.025, save_image = FALSE,
                        name = "empty")
{


  # NOTE: add warnings about size of grid and gridline size
  # DO: calculate gridline size as a proportion of the matrix size


  grid_to_draw <- compute_plot_points(grid_matrix, gridline_size)

  # FUNCTION TO WRITE: based on the matrix, find edges and compute line points
  # perimeter_to_draw <- compute_perimeter_lines(grid_matrix, gridline_size)

  # TESTING:
  # (x, y) is a point, (xend, yend) is other point, line connects them
  # plot coords start top left and work right, then down
  # lines <- data.frame(
  #   x = c(0, 0),
  #   y = c(0, -1),
  #   xend = c(0, 0),
  #   yend = c(-1, -2),
  #   width = c(0.25)
  # )


  plot <- ggplot2::ggplot(grid_to_draw, aes(x = x, y = y)) +
    ggforce::geom_shape(aes(group = element),
                        fill = grid_to_draw$draw_colour
                        #expand = unit(1, 'cm'), # would add a border
                        #radius = unit(0.5, 'cm')  # not sure what radius does
    ) +
    # ggforce::geom_link0(data = lines, size = 2, aes(x = x, y = y,
    #           xend = xend, yend = yend))# +
    #geom_polygon(fill = 'black') +
    #xlim(-4, 1) + ylim(-5, 0) +
    ggplot2::theme_void() #+
  print(plot)

  if (name == "empty") {
    file_name <- paste0(#"a", calculate_area(grid_matrix),
      #"_p", calculate_perimeter(grid_matrix),
      #"_n", n, "_m", m, "_v",
      #paste(v, collapse = ''),
      as.character(rnorm(1,1)),
      ".png")
  } else {
    file_name <- name
  }

  if (save_image) {
    cowplot::ggsave2(filename = file_name,
                     height = 4, width = 4, dpi = 1600)
  }



}

  #grid_to_draw <- test_function(grid_matrix)


  # grid_to_draw$shape[grid_to_draw$element==14] <- "triangle"

  # grid_to_draw <- grid_to_draw %>%
  #   group_by(element) %>%
  #   slice(which(!(element == '14' & row_number() == 3)))




  # remove grid, axes, axis labels, etc
  # plot <- ggplot2::ggplot(grid_to_draw, aes(x = x, y = y)) +
  #   ggforce::geom_shape(aes(group = element),
  #              fill = grid_to_draw$draw_colour
  #              #expand = unit(1, 'cm'), # would add a border
  #              #radius = unit(0.5, 'cm')  # not sure what radius does
  #   ) +
  #   # ggforce::geom_link(data = lines, size = 2, aes(x = x, y = y,
  #   #           xend= xend, yend = yend)) +
  #   #geom_polygon(fill = 'black') +
  #   #xlim(-4, 1) + ylim(-5, 0) +
  #   ggplot2::theme_void() #+
  # print(plot)

  # THIS ASPECT ISNT RIGHT YET
    # the complex values put things off a bit


  # note: for complex, maybe make another vector of:
    # spaces below the values
    # so c(0, 2, 3, 0, 1) would have boxes drawn at:
      # bottom in col 1
      # 3 up in col 2
      # 4 up in col 3
      # bottom in col 4
      # 1 up in col 5




# theme(axis.title.x = element_blank()) +
# theme(axis.title.y = element_blank()) +
# theme(axis.text = element_blank()) +
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# # starting parameters
# shape <- "square"
# element <- 1
# box_colour <- "blue"
# background_colour <- "white"
#
# # NOTE: I have N and M reversed here compared to create_matrix
# n <- nrow(grid_matrix)  # rows
# m <- ncol(grid_matrix)  # columns
# v <- rowSums(grid_matrix)
# # want a function to calculate the spaces below for the complex ones
#
# size <- 1
# axis_bounds <- n + m + 1  # this will need to be changed most likely
#
# x_increment <- size * 1.025
# y_increment <- size * 1.025
#
# if (shape == "square") {
#   num_of_coords <- 4
# }
#
# grid <- data.frame(matrix(nrow = 0, ncol = 4))
# colnames(grid) <- c("element", "shape", "x", "y")
# grid$shape <- as.character(grid$shape)
#
#
# # set initial y coord
# y_start <- -1
#
#
#
# for (i in 1:n) {
#
#   # reset initial x coord
#   x_start <- -1
#
#   for (j in 1:m) {
#
#     # order of points: bottom left, bottom right, top right, top left
#     grid_cell <- data.frame(
#       element = c(rep(element, num_of_coords)),
#       shape = c(rep(shape, num_of_coords)),
#       x = c(x_start,
#             x_start+size,
#             x_start+size,
#             x_start),
#       y = c(y_start-size,
#             y_start-size,
#             y_start,
#             y_start)
#     )
#
#     grid <- bind_rows(grid, grid_cell)
#
#     element <- element + 1
#
#     # increment x to move grid cell right
#     x_start <- x_start - x_increment
#
#   }  # end m loop
#
#   # decrement y to move grid cell down
#   y_start <- y_start - y_increment
#
# }  # end n loop
#
#
#
# grid$draw_colour <- rep(as.vector(
#   t(t(rotate(rotate(grid_matrix))))), each = 4)  # grid matrix is from another script
# grid$draw_colour[grid$draw_colour == 1] <- box_colour  #"blue"
# grid$draw_colour[grid$draw_colour == 0] <- background_colour  #"white"
