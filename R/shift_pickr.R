#' Chemical Shift Picking
#'
#' @param x
#' @param p
#' @param sh
#' @param pm
#'
#' @return
#' @export
#'
#' @examples

shift_pickr <- function(x, p, sh, pm){
  if (length(sh)>2){
    i <- sh
  } else {
    i <- get_idx(sh, p)
  }
  xt <- x[1,]
  m <- unname(which(xt==max(xt[i])))
  s <- get_idx(c(p[m]-pm,p[m]+pm), p)
  return(s)
}
