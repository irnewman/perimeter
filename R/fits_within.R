
fits_within <- function(inner, outer)
{
  fits_within <- TRUE
  for (i in 1:nrow(outer)) {
    for (j in 1:ncol(outer)) {
      if (inner[i, j] == 1 & outer[i, j] != 1) {
        fits_within <- FALSE
      }
    }
  }

  return(fits_within)
}
