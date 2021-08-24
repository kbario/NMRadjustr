#' NMR Number of Scans Estimator
#'
#' @param sig The signal
#' @param noi The noise
#' @param goal The target signal to noise
#' @param ns The number of scans used to create the provided signal and noise values
#' @param r2 Arguments passed to embedded roundr function. See roundr help for details.
#'
#' @return The number of scans (rounded based on the input arguments from r2) necessary to achieve the goal signal to noise
#' @export
#'
#' @examples
adjustr <- function(sig, noi, goal, ns, r2){
  roundr(((goal/(sig/noi))^2)*ns, r2)
}
