#' Total Area Estimation
#'
#' @param x The spectrum of which you want to calculate the total area
#' @param ppm The matched ppm variable to x
#' @param n The noise estimation from the pproc
#' @param sh The chemical shift of the region you want to calculate total area of
#'
#' @return
#' @export
#'
#' @examples

tota <- function(x, p, n, sh = c(0.25,9.5)){
  xt <- x[1,]
  xt[xt<n]=0
  s <- sum(xt)
  return(s)
}
