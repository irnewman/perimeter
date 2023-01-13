
# samples is in this case, the number of sides
  # changing this so it doesn't require sides
  # but the function should still work as is, just with 2 sides max
# if samples is 1, only one side/value, so that must be n

sum_to_n <- function(n, samples, max_value = 10)
{
  finished <- FALSE

  while(!finished) {
    range <- c(-n:n)

    if (samples > 1) {
      values <- sample(range, samples-1)
      last_value <- n - sum(values)
      values <- c(values, last_value)
    } else {
      values <- n
    }

    if (all(values > -max_value)
        & all(values < max_value)) {
      finished <- TRUE
    }
  }

  return(values)
}
