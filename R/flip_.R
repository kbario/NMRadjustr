#' Spectral Flipping
#'
#' @param x the spectra wanting to be flipped
#' @param p the match ppm variable
#' @param sh the chemical shift used to calculate orientation
#' @author Kyle Bario \email{kylebario1@@gmail.com}
#' @return Returns a spectrum (array) with the correct orientation
#' @export
#'
#' @examples
flip_ <- function(x, p, sh){
  s = sum(x[get_idx(sh, p)])
  if (s<=0){
    x <- x*-1
  }
  return(x)
}
