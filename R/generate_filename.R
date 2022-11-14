
generate_filename <- function(grid_size,
                              area,
                              width,
                              height,
                              perimeter,
                              complexity,
                              flat_sides,
                              matrix_string)
{

  filename <- paste0(
    "g", grid_size, "_",
    "a", area, "_",
    "w", width, "_",
    "h", height, "_",
    "p", perimeter, "_",
    "c", complexity, "_",
    "f", flat_sides, "_",
    "ms", matrix_string
  )



  return(filename)
}

