
count_similarity <- function(m1, m2)
{
  row_wise <- FALSE
  col_wise <- FALSE

  if ( (ncol(m1) != ncol(m2) )
       | (nrow(m1) != nrow(m2)) ) {
    print("PERIMETER: error, matrices of unequal size")
    return()
  }

  total_matches <- 0

  if (ncol(m1) < nrow(m2)) {
    row_wise <- TRUE
  } else {
    col_wise <- TRUE
  }

  if (col_wise) {
    for (i in 1:(ncol(m1))) {

      m_row <- m1[, i]
      b_row <- m2[, i]

      row_compare <- match(m_row, 1) == match(b_row, 1)
      row_matches <- length(which(row_compare))

      total_matches <- total_matches + row_matches
    }
  }

  if (row_wise) {
    for (j in 1:(nrow(m1))) {

      m_col <- m1[j, ]
      b_col <- m2[j, ]

      col_compare <- match(m_col, 1) == match(b_col, 1)
      col_matches <- length(which(col_compare))

      total_matches <- total_matches + col_matches
    }
  }

  return(total_matches)
}
