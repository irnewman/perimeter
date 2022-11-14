
# NOTE: returns a list of 4 offsets

# note, parameters are just 1's and 0's instead now
# 1 = extra padding on that side when odd value

determine_padding <- function(width, height, grid_size,
                              top = 0, bottom = 1, left = 0, right = 1)
{

  # return if parameters incompatible
  if (width > grid_size) {
    print("PERIMETER: shape size and grid size are incompatible")
    print("PERIMETER: grid size must be larger than width")
    return()
  }
  if (height > grid_size) {
    print("PERIMETER: shape size and grid size are incompatible")
    print("PERIMETER: grid size must be larger than height")
    return()
  }

  # default is shift up and left
  if (top <= bottom) {
    v_off <- 1
  } else {
    v_off <- -1
  }
  if (left <= right) {
    h_off <- 1
  } else {
    h_off <- -1
  }

  # if true, width is centered evenly
  if ( (grid_size - width)%%2 == 0) {
    p_width <- (grid_size - width) / 2
    p_left <- p_width
    p_right <- p_width
  # else, we want to shift to one side by 1 row, based on h_offset argument
  } else {
      # print warnings
      # print("PERIMETER: warning, grid size minus width should be even")
      # print("PERIMETER: shape will not be centered in grid")
      # print("PERIMETER: specify centering with h_offset")

    x <- ceiling(grid_size - width)
    p_left <- (x - h_off)/2
    p_right <- (x + h_off)/2
  }

  # if true, height is centered evenly
  if ( (grid_size - height)%%2 == 0) {
    p_height <- (grid_size - height) / 2
    p_top <- p_height
    p_bottom <- p_height
  } else {
    # print warnings
    # print("PERIMETER: warning, grid size minus height should be even")
    # print("PERIMETER: shape will not be centered in grid")
    # print("PERIMETER: specify centering with v_offset")
    y <- ceiling(grid_size - height)
    p_top <- (y - v_off)/2
    p_bottom <- (y + v_off)/2
  }

  # so that list entries have names
  return(list(top = p_top,
              bottom = p_bottom,
              left = p_left,
              right = p_right))
}


  # if (shape == "square") {
  #   if ( (grid_size - n)%%2 == 0) {
  #     p <- (grid_size - n) / 2
  #     p_top <- p
  #     p_bottom <- p
  #     p_left <- p
  #     p_right <- p
  #   } else {
  #     # print warnings
  #     print("PERIMETER: warning, grid size minus n should equal an even number")
  #     print("PERIMETER: shape will not be centered in grid")
  #     print("PERIMETER: specify centering with v_offset and h_offset")
  #     x <- (grid_size - n)
  #     p_top <- (x - v_off)/2
  #     p_bottom <- (x + v_off)/2
  #     p_left <- (x - h_off)/2
  #     p_right <- (x + h_off)/2
  #   }
  # }



  # if (shape == "rectangle") {
  #
  #   rect_height <- n
  #   rect_length <- m
  #
  #   if (n > m) {
  #     vert_p <- p
  #     horiz_p <- p + as.integer((n-m)/2)  # rounds to lowest integer
  #   } else {
  #     vert_p <- p + as.integer((m-n)/2)
  #     horiz_p <- p
  #   }
  #
  #
  #
  #   if ( (grid_size - n)%%2 == 0
  #        | (grid_size - m)%%2 == 0) {
  #     p <- (grid_size - n) / 2
  #     p_top <- p
  #     p_bottom <- p
  #     p_left <- p
  #     p_right <- p
  #   } else {
  #     # print warnings
  #     print("PERIMETER: warning, grid size minus n should equal an even number")
  #     print("PERIMETER: shape will not be centered in grid")
  #     print("PERIMETER: specify centering with v_offset and h_offset")
  #
  #     if (v_offset == "top") {
  #       v_off <- 1
  #     } else {
  #       v_off <- -1
  #     }
  #
  #     if (h_offset == "left") {
  #       h_off <- 1
  #     } else {
  #       h_off <- -1
  #     }
  #
  #     x <- (grid_size - n)
  #     p_top <- (x - v_off)/2
  #     p_bottom <- (x + v_off)/2
  #     p_left <- (x - h_off)/2
  #     p_right <- (x + h_off)/2
  #   }
  # }





  # new_p <- p
  # vert_offset <- 0
  #
  #
  # if ( (n + (p*2) != grid_size ) ) {
  #   print("PERIMETER: warning, specified values exceed grid area")
  #   print("PERIMETER: padding has been reduced")
  #   new_p <- p - 1
  # }
  #
  # if ( (n + (p*2) < (grid_size - 2)  )) {
  #   new_p <- p + 1
  # }
  #
  # if ( (n + (p*2))%%2 != 0) {  # doesn't divide evenly
  #   # add this to n_offset error from generate rect then
  # }


  # return(list(p = p, vert_offset = vert_offset, horiz_offset = horiz_offset))

