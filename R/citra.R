#' Citrate Area Estimation
#'
#' @param x The spectrum of which you want to calculate the total area
#' @param p The matched ppm variable to x
#' @param n The noise estimation from the pproc
#' @param sh The ppm lower and upper limits defining the cirtate signal
#'
#' @return An estimated value of the citrate area
#' @export
#'
#' @examples

citra <- function(x, p, n, sh = c(5.2,5.3)){
  b <- x[1,shift_pickr(x, p, sh, 0.005)]
  b[b<n]=0
  i <- get_idx(sh, p)
  j <- i[-(match(b,i))]
  s <- x[1,shift_pickr(x, p, j, 0.005)]
  s[s<n]=0
  sm <- sum(x[c(b, s)])
  return(sm)
}

