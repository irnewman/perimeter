
# REWRITE
# split vector into most frequent and the rest
  # but also account for there being two or more equal (which would auto fail?)
# check all that are not the most frequent
  # if more than 1 away, they are on the adjacent side
  # if they are 1 away, it's uneven (or do we allow for corners? probably not)
  # if any are interspersed within the most frequent, it is uneven


# might be easiest to set very strict rules for what counts as flat
  # then all else are not flat
  # 1) all indices on side are equal
  # 2) if not, then most frequent indices on side are uninterrupted
    # a) and any others must be toward inside and no more than 1 single value
    # b) or more than 1 value toward inside (means they are on different side)


check_sides <- function(matrix, check = "flat_count")
{
  reduced <- trim_matrix(matrix)
  left <- c()
  right <- c()
  top <- c()
  bottom <- c()
  flat_count <- 0
  uneven_count <- 0
  flat_sides <- c()
  uneven_sides <- c()

  # left
  for (a in 1:nrow(reduced)) {
    current_a <- reduced[a, ]
    left <- c(left, min(which(current_a != 0)))
  }
  # right
  for (b in 1:nrow(reduced)) {
    current_b <- reduced[b, ]
    right <- c(right, max(which(current_b != 0)))
  }
  # top
  for (c in 1:ncol(reduced)) {
    current_c <- reduced[, c]
    top <- c(top, min(which(current_c != 0)))
  }
  # bottom
  for (d in 1:ncol(reduced)) {
    current_d <- reduced[, d]
    bottom <- c(bottom, max(which(current_d != 0)))
  }

  left_flat <- check_flatness(left, "left")
  right_flat <- check_flatness(right, "right")
  top_flat <- check_flatness(top, "top")
  bottom_flat <- check_flatness(bottom, "bottom")


  if (left_flat) {
    flat_count <- flat_count + 1
    flat_sides <- paste0(flat_sides, "L")
  } else {
    uneven_count <- uneven_count + 1
    uneven_sides <- paste0(uneven_sides, "L")
  }
  if (right_flat) {
    flat_count <- flat_count + 1
    flat_sides <- paste0(flat_sides, "R")
  } else {
    uneven_count <- uneven_count + 1
    uneven_sides <- paste0(uneven_sides, "R")
  }
  if (top_flat) {
    flat_count <- flat_count + 1
    flat_sides <- paste0(flat_sides, "T")
  } else {
    uneven_count <- uneven_count + 1
    uneven_sides <- paste0(uneven_sides, "T")
  }
  if (bottom_flat) {
    flat_count <- flat_count + 1
    flat_sides <- paste0(flat_sides, "B")
  } else {
    uneven_count <- uneven_count + 1
    uneven_sides <- paste0(uneven_sides, "B")
  }

  if (flat_count == 0) {
    flat_sides <- "N"
  }
  if (uneven_count == 0) {
    uneven_sides <- "N"
  }

  if (check == "flat_count") {
    return(flat_count)
  }
  if (check == "uneven_count") {
    return(uneven_count)
  }
  if (check == "flat_sides") {
    return(flat_sides)
  }
  if (check == "uneven_sides") {
    return(uneven_sides)
  }
}





# if (length(unique(left)) == 1) {
#   left_flat <- TRUE
# } else if (length(unique(left)) > 1) {
#   # find most common index of side
#   left_freqs <- data.frame(sort(table(left), decreasing = TRUE))
#   left_vec <- as.numeric(as.character(left_freqs$left[1]))
#   l_counter <- 1  # counter for loop
#   l_unflat <- 0   # instances of uneven
#   for (l in left) {
#     if ((l_counter == 1) | (l_counter == length(left))) {  # if first/last
#       if (l < left_vec) {
#         l_unflat <- l_unflat + 1
#       }
#     } else if (l != left_vec) {
#       l_unflat <- l_unflat + 1
#     }
#     l_counter <- l_counter + 1
#   }
#   if (l_unflat > 0) {
#     left_flat <- FALSE
#   } else {
#     left_flat <- TRUE
#   }
#   if (as.numeric(as.character(left_freqs$Freq[1]))
#       == as.numeric(as.character(left_freqs$Freq[2]))
#   ) {
#     left_flat <- FALSE
#   }
# }
#
# if (length(unique(right)) == 1) {
#   right_flat <- TRUE
# } else if (length(unique(left)) > 1) {
#   # find most common index of side
#   right_freqs <- data.frame(sort(table(right), decreasing = TRUE))
#   right_vec <- as.numeric(as.character(right_freqs$right[1]))
#   r_counter <- 1  # counter for loop
#   r_unflat <- 0   # instances of uneven
#   for (r in right) {
#     # THIS DOESNT WORK PROPERLY, NEEDS TO BE THE 1st BEFORE the most freq
#     if ((r_counter == 1) | (r_counter == length(right))) {  # if first/last
#       if (r > right_vec) {
#         r_unflat <- r_unflat + 1
#       }
#     } else if (r != right_vec) {
#       r_unflat <- r_unflat + 1
#     }
#     r_counter <- r_counter + 1
#   }
#   if (r_unflat > 0) {
#     right_flat <- FALSE
#   } else {
#     right_flat <- TRUE
#   }
#   if (as.numeric(as.character(right_freqs$Freq[1]))
#       == as.numeric(as.character(right_freqs$Freq[2]))
#   ) {
#     right_flat <- FALSE
#   }
# }
#
# if (length(unique(top)) == 1) {
#   top_flat <- TRUE
# } else if (length(unique(top)) > 1) {
#   # find most common index of side
#   top_freqs <- data.frame(sort(table(top), decreasing = TRUE))
#   top_vec <- as.numeric(as.character(top_freqs$top[1]))
#   t_counter <- 1  # counter for loop
#   t_unflat <- 0   # instances of uneven
#   for (t in top) {
#     if ((t_counter == 1) | (t_counter == length(top))) {  # if first/last
#       if (t < top_vec) {
#         t_unflat <- t_unflat + 1
#       }
#     } else if (t != top_vec) {
#       t_unflat <- t_unflat + 1
#     }
#     t_counter <- t_counter + 1
#   }
#   if (t_unflat > 0) {
#     top_flat <- FALSE
#   } else {
#     top_flat <- TRUE
#   }
#   if (as.numeric(as.character(top_freqs$Freq[1]))
#       == as.numeric(as.character(top_freqs$Freq[2]))
#   ) {
#     top_flat <- FALSE
#   }
# }
#
# if (length(unique(bottom)) == 1) {
#   bottom_flat <- TRUE
# } else if (length(unique(bottom)) > 1) {
#   # find most common index of side
#   bottom_freqs <- data.frame(sort(table(bottom), decreasing = TRUE))
#   bottom_vec <- as.numeric(as.character(bottom_freqs$bottom[1]))
#   b_counter <- 1  # counter for loop
#   b_unflat <- 0   # instances of uneven
#   for (b in bottom) {
#     if ((b_counter == 1) | (b_counter == length(bottom))) {  # if first/last
#       if (b > bottom_vec) {
#         b_unflat <- b_unflat + 1
#       }
#     } else if (b != bottom_vec) {
#       b_unflat <- b_unflat + 1
#     }
#     b_counter <- b_counter + 1
#   }
#   if (b_unflat > 0) {
#     bottom_flat <- FALSE
#   } else {
#     bottom_flat <- TRUE
#   }
#   if (as.numeric(as.character(bottom_freqs$Freq[1]))
#       == as.numeric(as.character(bottom_freqs$Freq[2]))
#   ) {
#     bottom_flat <- FALSE
#   }
# }
