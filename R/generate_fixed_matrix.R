
generate_fixed_matrix <- function(v, grid_size = 10)
{
  # if ( (length(v) + p*2) > grid_size ) {
  #   print("PERIMETER: vector plus padding larger than grid size")
  #   return()
  # }
  #
  # if ( length(v) + p*2 != grid_size ) {
  #   print("PERIMETER: warning, inconsistent sizes will mean shape not centered")
  # }

  return(fill_matrix(grid_size, v))
}
