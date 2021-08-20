#' Baseline Correction
#'
#' @param x the spectrum to be baseline corrected
#'
#' @return an x with a corrected baseline
#' @importFrom ptw asysm
#' @export
#'
#' @examples

bl_ <- function(x){
  if (any(is.na(x))){
    x[is.na(x)]=0
  }
  xb <- x-asysm(x, maxit = 30, lambda = 1e+07)
  return(xb)
}
