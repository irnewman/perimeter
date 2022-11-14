generate_baseline <- function(shape, size)
{
  if (shape == "square") {
    grid_matrix <- generate_baseline_shape(width = size, height = size,
                                           grid_size = 10,
                                           v_offset = "top", h_offset = "left")
  }

  if (shape == "rectangle_high") {
    grid_matrix <- generate_baseline_shape(width = size-2, height = size,
                                           grid_size = 10,
                                           v_offset = "top", h_offset = "left")
  }

  if (shape == "rectangle_wide") {
    grid_matrix <- generate_baseline_shape(width = size, height = size-2,
                                           grid_size = 10,
                                           v_offset = "top", h_offset = "left")
  }

  return(grid_matrix)
}

