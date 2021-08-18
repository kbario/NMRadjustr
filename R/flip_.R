#' Spectral Flipping
#'
#' @param x the spectra wanting to be flipped
#' @param ppm the match ppm variable
#' @param shift the chemical shift used to calculate orientation
#' @author Kyle Bario \email{kylebario1@@gmail.com}
#' @return
#' @export
#'
#' @examples
flip_ <- function(x, p, sh){
  idx = get_idx(sh, p)
  s = sum(x[idx])
  if (s<=0){
    x <- x*-1
  }
  return(x)
}
