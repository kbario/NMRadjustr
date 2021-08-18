#' Title
#'
#' @param x
#' @param ppm
#' @param ta
#' @param n
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
