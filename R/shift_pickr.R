#' Chemical Shift Picking
#'
#' @param x The spectrum of which you want to calculate the total area
#' @param ppm The matched ppm variable to x
#' @param sh The ppm lower and upper limits defining the region you want to search in
#' @param pm The plus/minus value you want to add or subtract from the peak. Default = 0.005
#'
#' @return
#' @export
#'
#' @examples

shift_pickr <- function(x, p, sh, pm = 0.005){
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
