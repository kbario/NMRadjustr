#' Baseline Correction
#'
#' @param x the spectrum to be baseline corrected
#'
#' @return an x with a corrected baseline
#' @export
#'
#' @examples

bl_ <- function(x){
  if (is.null(ncol(x))){
    x <- t(x)
    return(x)
  }
  xb <- t(apply(x, 1, function(i){
    x <- ptw::asysm(x, maxit = 30, lambda = 1e+07)
  }))
  return(xb)
}
