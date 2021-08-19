#' Spectral Preprocessing
#'
#' @param x The spectra that you are processing
#' @param p The ppm matched to your spectra
#'
#' @return This function returns:
#' \itemize{
#'   \item **x**: a fully preprocessed spectrum ready for it's dilution to be estimated
#'   \item **ppm**: the matched ppm variable for the preprocessed x
#'   \item **x_og**: the original X spectra, only processed by the NMR
#' @export
#'
#' @example pproc(x,p)

pproc <- function(x, p){
  xf <- flip_(x, p, c(3,3.1))
  x_og <- xf
  n <- noi_(x,p)
  xr <- xf[-c(get_idx(c(min(p), 0.25), p), get_idx(c(4.6,4.9), p), get_idx(c(5.55,6), p), get_idx(c(9.5,max(p)), p))]
  pn <- p[-c(get_idx(c(min(p), 0.25), p), get_idx(c(4.6,4.9), p), get_idx(c(5.55,6), p), get_idx(c(9.5,max(p)), p))]
  xb <- bl_(xr)
  assign('x', xb, envir = .GlobalEnv)
  assign('p', pn, envir = .GlobalEnv)
  assign('x_og', x_og, envir = .GlobalEnv)
  assign('noi', n, envir = .GlobalEnv)
}
