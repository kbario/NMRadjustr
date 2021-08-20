#' NMR spectral TSP calibration
#'
#' @param x The spectrum you want to calibrate as an array
#' @param p The matched ppm variable to the x you want to calibrate
#'
#' @return A spectrum that is calibrated to tsp
#' @export
#'
#' @examples

cali <- function(x, p){
  if (!is.null(dim(x))){
    stop('This function is intended to only calibrate a single spectra')
  }
  i <- get_idx(c(-.2,.2), p)
  d <- (which.min(abs(p)))-(which(p==p[i][which.max(x[i])]))
  si <- sign(d)
  dif <- abs(d)
  if (si==-1){
    x <- x[-(1:dif)]
    x <- c(x, rep(0, dif))
  }
  if (si==1){
    x <- x[-((length(x)-dif+1):length(x))]
    x <- c(rep(0, dif), x)
  }
  return(x)
}
