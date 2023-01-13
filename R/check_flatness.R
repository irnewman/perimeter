
# adjusting this to be more strict, won't affect the check sides function

check_flatness <- function(v, side, method = "strict")
{

  if (method == "strict") {
    if (length(unique(v)) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  if (method == "moderate") {
    if (length(unique(v)) == 1) {
      return(TRUE)
    } else if (length(unique(v)) > 1) {

      # find most common index of side
      v_freqs <- data.frame(sort(table(v), decreasing = TRUE))
      v_most <- as.numeric(as.character(v_freqs$v[1]))

      # count sequence of most common
      sequences <- rle(v)
      len_most <- sequences$lengths[sequences$values == v_most]
      most_position <- which(sequences$values == v_most)

      # if more than 1 instance of most common, then the side is interrupted
      if (length(len_most) > 1) {
        return(FALSE)
      }

      # if any are more towards outside than flattest part of side, not flat
        # or more than 2 total sequences (so 1 side must be a nice corner)
      if (side == "top" | side == "left") {
        if (any(sequences$values < v_most)
            | length(sequences$lengths) > 2
            #| any(sequences$values == (v_most+1))
            ) {
          return(FALSE)
        }
      }
      if (side == "bottom" | side == "right") {
        if (any(sequences$values > v_most)
            | length(sequences$lengths) > 2
            #| any(sequences$values == (v_most-1))
            ) {
          return(FALSE)
        }
      }
    }
    # every other instance returns true
    return(TRUE)
  }


  if (method == "lenient") {
    if (length(unique(v)) == 1) {
      return(TRUE)
    } else if (length(unique(v)) > 1) {

      # find most common index of side
      v_freqs <- data.frame(sort(table(v), decreasing = TRUE))
      v_most <- as.numeric(as.character(v_freqs$v[1]))

      # count sequence of most common
      sequences <- rle(v)
      len_most <- sequences$lengths[sequences$values == v_most]
      most_position <- which(sequences$values == v_most)

      # if more than 1 instance of most common, then the side is interrupted
      if (length(len_most) > 1) {
        return(FALSE)
      }

      # if any are more towards outside than flattest part of side, not flat
      if (side == "top" | side == "left") {
        if (any(sequences$values < v_most)) {
          return(FALSE)
        }
      }
      if (side == "bottom" | side == "right") {
        if (any(sequences$values > v_most)) {
          return(FALSE)
        }
      }

      # check left side
      if (most_position > 1) {
        if (sequences$lengths[most_position - 1] > 1) {
          return(FALSE)
        }
        # if (sequences$lengths[most_position - 1] == 1
        #     & abs(sequences$values[most_position] -
        #           sequences$values[most_position - 1]) == 1) {
        #   return(FALSE)
        # }
      }

      # check right side
      if (most_position < length(sequences$values)) {
        if (sequences$lengths[most_position + 1] > 1) {
          return(FALSE)
        }
        # if (sequences$lengths[most_position + 1] == 1
        #     & abs(sequences$values[most_position] -
        #           sequences$values[most_position + 1]) == 1) {
        #   return(FALSE)
        # }
      }

      if (most_position > 1
          & most_position < length(sequences$values)) {
        if (sequences$lengths[most_position - 1] == 1
            & sequences$lengths[most_position + 1] == 1) {
          return(FALSE)
        }
      }
    }

    # every other instance returns true
    return(TRUE)
  }

}








# check sequences$values at +/- 1 from y, unless y = 1 or y = length(seq)
# and check the difference of value from most freq value



# if there is only 1 sequence of most common, then values next to must be
# either 2 or more toward the "inside" of the shape
# or only 1 "wide" before the next also moves further to inside


# either next sequence beyond is only length 1
# or the next sequence value is more than 1 away



# l_counter <- 1  # counter for loop
# l_unflat <- 0   # instances of uneven
# for (l in left) {
#   if ((l_counter == 1) | (l_counter == length(left))) {  # if first/last
#     if (l < left_vec) {
#       l_unflat <- l_unflat + 1
#     }
#   } else if (l != left_vec) {
#     l_unflat <- l_unflat + 1
#   }
#   l_counter <- l_counter + 1
# }
# if (l_unflat > 0) {
#   left_flat <- FALSE
# } else {
#   left_flat <- TRUE
# }
# if (as.numeric(as.character(left_freqs$Freq[1]))
#     == as.numeric(as.character(left_freqs$Freq[2]))
# ) {
#   left_flat <- FALSE
# }
