#' Round to a Multiple of 4
#'
#' @param n The number you want to round
#' @param r2 Encodes the:
#' 1. minimum value to be rounded to
#' 2. maximum value to be rounded to, and
#' 3. the step/multiple
#'
#' @return A integer rounded to the nearest multiple of
#' @export
#'
#' @examples
roundr <- function(n, r2 = c(4, 512, 4)){
  sq <- seq(r2[1], r2[2], r2[3])
  sq[which.min(abs(sq-n))]
}
