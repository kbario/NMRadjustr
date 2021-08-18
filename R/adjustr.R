#' Title
#'
#' @param sig The signal
#' @param noi The noise
#' @param goal The target signal to noise
#' @param ns The number of scans used to create the provided signal and noise values
#'
#' @return The number of scans (rounded to the nearest multiple of 4 capped at 512) necessary to achieve the goal signal to noise
#' @export
#'
#' @examples
adjustr <- function(sig, noi, goal, ns){
  roundr(((goal/(sig/noi))^2)*ns)
}
