#' Round to a Multiple of 4
#'
#' @param n The number you want to round
#'
#' @return
#' @export
#'
#' @examples
roundr <- function(n){
  sq <- seq(4,512,4)
  sq[which.min(abs(sq-n))]
}
