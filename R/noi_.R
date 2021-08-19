#' Noise Estimation
#'
#' @param x The read in spectra
#' @param p the matched ppm var to your x
#'
#' @return an estimation of noise level
#' @export
#' @importFrom stats sd
#'
#' @examples

noi_ <- function(x, p){
  rm <- 5*sd(x[get_idx(c(9.5,11), p)])+(mean((x[get_idx(c(9.5,11), p)]), trim = 0.05))
  return(rm)
}
