
# FINISHED: add documentation next

# returns:
  # padding = number of 1's? p - this value I guess?
  # middle = number of 1's for sure


sample_placement <- function(n, p, type)
{
  if (p >= 4) {
    print("PERIMETER: currently does not support padding higher than 4")
    return()
  }

  # NOTE: can weight the sampling vector with an argument eventually
  if (type == "padding") {
    padded_weights <- c(0.65, 0.275, 0.025, 0.025, 0.025)

    values <- c(rep(0:p))
    weights <- padded_weights[1:(p+1)]

    return(sample(values, 1, prob = weights))
  }

  if (type == "middle") {
    values <- c(rep(1:(n-1)))
    return(sample(values, 1))
  }
}
