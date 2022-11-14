
# note: could compare other types of symmetry

# unfinished: need to record the optimal score in loop


compute_symmetry <- function(matrix_l, matrix_r)
{
  # 1: test without trimming first
  flipped <- flip_matrix(matrix_r, direction = "horizontal")
  best_symmetry <- count_similarity(matrix_l, flipped)


  # 2: if not perfectly aligned, need to test with trimming shapes

  # remove empty space around shapes
  ml <- trim_matrix(matrix_l)
  mr <- trim_matrix(matrix_r)

  row_diff <- abs(nrow(ml) - nrow(mr))
  if (nrow(ml) > nrow(mr)) {
    row_adjust <- "mr"
  }
  if (nrow(ml) < nrow(mr)) {
    row_adjust <- "ml"
  }
  if (row_diff == 0) {
    row_adjust <- "none"
  }

  col_diff <- abs(ncol(ml) - ncol(mr))
  if (ncol(ml) > ncol(mr)) {
    col_adjust <- "mr"
  }
  if (ncol(ml) < ncol(mr)) {
    col_adjust <- "ml"
  }
  if (col_diff == 0) {
    col_adjust <- "none"
  }

  for (i in 1:(row_diff+1)) {
    top <- i - 1
    bottom <- row_diff - top
    for (j in 1:(col_diff+1)) {
      left <- j - 1
      right <- col_diff - left

      m1 <- ml
      m2 <- mr

      # add rows
      if (row_adjust == "ml") {
        m1 <- increase_matrix(m1,
                              top = top,
                              bottom = bottom)
      }
      if (row_adjust == "mr") {
        m2 <- increase_matrix(m2,
                              top = top,
                              bottom = bottom)
      }

      # add cols
      if (col_adjust == "ml") {
        m1 <- increase_matrix(m1,
                              left = left,
                              right = right)
      }
      if (col_adjust == "mr") {
        m2 <- increase_matrix(m2,
                              left = left,
                              right = right)
      }


      # symmetry is flip the matrix around y-axis, then count matches
      f <- flip_matrix(m2, direction = "horizontal")
      current_symmetry <- count_similarity(m1, f)
      if (current_symmetry > best_symmetry) {
        best_symmetry <- current_symmetry
      }
    }
  }

  return(best_symmetry)

}

  # if (row_adjust == "m2" & col_adjust == "m1") {
  #
  # }






  # take one shape and flip it around y-axis


  # call count_differences on the two
  #ymmetry <- count_differences(m1, f)





# that means that symmetry is 0 when identical, low when similar, high when very diff
#     m2 <- increase_matrix(m2, top = rows_to_add)
# m1 <- increase_matrix(m1, top = rows_to_add)




#
# row_diff <- nrow(m1) - nrow(m2)
#
# for (i in 1:(row_diff+1)) {
#   top <- i - 1
#   bottom <- row_diff - top
#
#   if (ncol(m1) > ncol(m2)) {
#     col_diff <- ncol(m1) - ncol(m2)
#
#     for (j in 1:(col_diff+1)) {
#       left <- j - 1
#       right <- col_diff - left
#
#       # m2 smaller cols
#       m2 <- increase_matrix(m2,
#                             top = top,
#                             bottom = bottom,
#                             left = left,
#                             right = right)
#       f <- flip_matrix(m2, direction = "horizontal")
#
#       current_symmetry <- count_differences(m1, f)
#     }
#   } else {
#     col_diff <- ncol(m2) - ncol(m1)
#
#     for (j in 1:(col_diff+1)) {
#       left <- j - 1
#       right <- col_diff - left
#
#       # m2 smaller cols
#       m1 <- increase_matrix(m1,
#                             top = top,
#                             bottom = bottom,
#                             left = left,
#                             right = right)
#       f <- flip_matrix(m1, direction = "horizontal")
#
#       current_symmetry <- count_differences(m2, f)
#     }
#   }
#
# }
