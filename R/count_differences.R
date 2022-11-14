
# take two matrices, the shape created and the baseline comparison
# row by row, count how many differences and return that value
# that value is the "complexity" index I'll use
# how different the shape is from a square/rectangle baseline


count_differences <- function(m, b)
{
  row_wise <- FALSE
  col_wise <- FALSE

  if ( (ncol(m) != ncol(b) )
       | (nrow(m) != nrow(b)) ) {
    print("PERIMETER: error, matrix and baseline comparison of unequal size")
    return()
  }

  total_differences <- 0

  if (ncol(m) < nrow(m)) {
    row_wise <- TRUE
  } else {
    col_wise <- TRUE
  }

  if (col_wise) {
    for (i in 1:(ncol(m))) {

      m_row <- m[, i]
      b_row <- b[, i]

      row_compare <- m_row == b_row
      row_differences <- length(row_compare[row_compare == FALSE])

      total_differences <- total_differences + row_differences
    }
  }

  if (row_wise) {
    for (j in 1:(nrow(m))) {

      m_col <- m[j, ]
      b_col <- b[j, ]

      col_compare <- m_col == b_col
      col_differences <- length(col_compare[col_compare == FALSE])

      total_differences <- total_differences + col_differences
    }
  }

  return(total_differences)
}

