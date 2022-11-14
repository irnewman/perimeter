
select_box <- function(vector, side, operation)
{
  box_positions <- which(vector == 1)

  if (side == "left" | side == "top") {
    if (operation == "subtraction") {
      box_to_change <- min(box_positions)
    }
    if (operation == "addition") {
      box_to_change <- min(box_positions) - 1
    }
  }
  if (side == "right" | side == "bottom") {
    if (operation == "subtraction") {
      box_to_change <- max(box_positions)
    }
    if (operation == "addition") {
      box_to_change <- max(box_positions) + 1
    }
  }
  return(box_to_change)
}


  # NOTE: taking this out for now, but may include later
    # it makes it so the additions can be not on the edge of side specified
    # it might be better to only do the edge though
  # if ( length(box_positions) > 1) {
  #   for (i in 2:length(box_positions)) {
  #     if ( (box_positions[i]-1) != box_positions[i-1] ) {
  #       boxes_to_add <- append(boxes_to_add, box_positions[i] + sign)
  #     }
  #   }
  # }
  # # select one 0 to change to 1
  # boxes_to_add <- unique(boxes_to_add)
  # sign <- 1 (was up earlier)

  # if (length(boxes_to_add) > 1) {
  #   box_to_add <- sample(boxes_to_add, 1)
  # } else {
  #   box_to_add <- boxes_to_add
  # }

