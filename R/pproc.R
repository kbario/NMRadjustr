#' Spectral Preprocessing
#'
#' @param x The spectra that you are processing
#' @param p The ppm matched to your spectra
#'
#' @return This function returns:
#' 1. **x**: a fully preprocessed spectrum ready for it's dilution to be estimated
#' 2. **ppm**: the matched ppm variable for the preprocessed x
#' 3. **x_og**: the original X spectra, only processed by the NMR
#' @export
#'
#' @example

pproc <- function(x, p){
  xf <- flip_(x, p, c(3,3.1))
  x_og <- xf
  n <- noi_(x,p)
  xr <- xf[-c(get_idx(c(min(p), 0.25), p), get_idx(c(4.6,4.9), p), get_idx(c(5.55,6), p))]
  pn <- p[-c(get_idx(c(min(p), 0.25), p), get_idx(c(4.6,4.9), p), get_idx(c(5.55,6), p), get_idx(c(9.5,max(p)), p))]
  xb <- bl_(xr)
  xg <- xb[,-get_idx(c(9.5,max(p)), p)]
  assign('x', xg, envir = .GlobalEnv)
  assign('p', pn, envir = .GlobalEnv)
  assign('o', x_og, envir = .GlobalEnv)
  assign('n', n, envir = .GlobalEnv)
}
