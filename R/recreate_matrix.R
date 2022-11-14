# take the string from the csv and turn it into a matrix
recreate_matrix <- function(matrix_string)
{
  if (sqrt(nchar(matrix_string)) %% 1 != 0) {
    return(print("only able to reconstruct with square dimensions"))
  } else {
    length <- sqrt(nchar(matrix_string))
  }


  v = as.numeric(stringr::str_split_fixed(matrix_string, "",
                                          n = nchar(matrix_string)))

  matrix <- data.frame(matrix(v, nrow = length, ncol = length))


  return(matrix)
}


