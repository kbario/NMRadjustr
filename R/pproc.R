#' Spectral Preprocessing
#'
#' @param x The spectra that you are processing
#'
#' @return This function returns a fully preprocessed spectra ready for it's dilution to be estimated
#' @export
#'
#' @examples

pproc <- function(x, p){
  x_og <- x
  xf <- flip_(x, p, shift = c(3,3.1))
  n <- noi_(x,p)
  xb <- bl_(xf)
  assign('X', xb, envir = .GlobalEnv)
  assign('X_OG', x_og, envir = .GlobalEnv)
  assign('noise', n, envir = .GlobalEnv)
}

flip_ <- function(X, ppm, shift = c(3, 3.1)){
  iid = get_idx(shift, ppm)
  out = t(apply(X, 1, function(x, idx=iid){
    if (sum(x[idx])<0) {
      x=x*-1
    }
    return(x)
  }))
}

noi_ <- function(X_OG, ppm_OG){
  rm <- apply(X_OG, 1, function(i){
    rm <- 5*stats::sd(i[get_idx(c(9.5,11), ppm_OG)])+(mean((i[get_idx(c(9.5,11), ppm_OG)]), trim = 0.05))
    return(rm)
  })
  return(rm)
}

bl_ <- function(x){
  if (is.null(ncol(x))){
    x <- t(x)
    return(x)
  }
  xb <- t(apply(x, 1, function(i){
    x <- ptw::asysm(x, maxit = 30, lambda = 1e+07)
  }))
  return(xb)
}
