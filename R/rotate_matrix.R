

rotate_matrix  <- function(x, clockwise=T) {
  if (clockwise) { t( apply(x, 2, rev))
  } else {apply( t(x),2, rev)}
}
