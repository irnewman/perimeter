# other that is a generate_item to make and save one grid as stimuli
# need gridline argument too


# a method to specify a range of values
  # area, perimeter, complexity
# DECIDE:
  # does this call perimeter matrix to get preciesely what I need
  # does this call within a range of values to find any solution
  # or is this just to save only, after the matrix is created?
    # that's what save item is for though

create_item <- function(lots_of_arguments)
{
  # 4 saving:
  # filenames (to fully describe the grid? not 100% possible)
  # maybe the csv can hold the matrix itself and then compare those?
  # so a function to clean up and remove all duplicates in folder? tricky

  perimeter <- calculate_perimeter(test_matrix)
  complexity <- min(c$complexity)
  width <- find_width(test_matrix)
  height <- find_height(test_matrix)
  flat_sides <- count_flat_sides(test_matrix)

  filename <- generate_filename(
    grid_size,
    area,
    width,
    height,
    perimeter,
    complexity,
    flat_sides,
    matrix_string
  )

  # save image
  cowplot::ggsave2(filename = paste0(filename, ".png"),
                   height = 4, width = 4, dpi = 1600)
  # save RDS
  saveRDS(test_matrix, file = paste0(filename, ".RDS"))

  q <- readRDS(paste0(filename, ".RDS"))
  # save csv
  csv <- data.frame(
    grid_size = grid_size,
    area = area,
    width = width,
    height = height,
    perimeter = perimeter,
    complexity = complexity,
    flat_sides = flat_sides,
    matrix_string = matrix_string
  ) # matrix string doesn't work properly with excel
  write.csv(csv, file = paste0(filename, ".csv"))

  # create/open folders, save rds, save image, save csv
  # opening and re-saving?
}



