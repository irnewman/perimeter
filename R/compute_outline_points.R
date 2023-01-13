
compute_outline_points <- function(matrix, grid_to_draw)
{
  outline_colour <- "black"
  # not sure why I need to change column types so much here
  outline <- data.frame(matrix(nrow = 0, ncol = ncol(grid_to_draw)))
  colnames(outline) <- colnames(grid_to_draw)
  outline$side <- as.character(outline$side)
  outline$shape <- as.character(outline$shape)
  outline$cell_colour <- as.character(outline$cell_colour)


  # NOTE WORKING YET
  for (ii in 1:nrow(matrix)) {
    for (jj in 1:ncol(matrix)) {
      if (matrix[ii, jj] == 1) {
        current_edges <- find_edges(matrix, ii, jj) # will find where cell has neighbours
        lines <- grid_to_draw %>%
          filter(
            i == ii
            & j == jj
            & !(side %in% current_edges)
          )
        outline <- dplyr::bind_rows(outline, lines)
      }
    }
  }
  # use matrix to compute which cell has which edges to be drawn (similar to gather indices)
    # only if has value 1

  # use that new matrix to select only parts of grid_to_draw as border to draw


  # return that df to be plotted separately by plot_matrix
  names(outline)[names(outline) == 'cell_colour'] <- 'outline_colour'
  outline$outline_colour <- outline_colour

  return(outline)
}
