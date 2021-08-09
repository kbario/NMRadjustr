pproc <- function(x){
  x_og <- x
  p <- as.numeric(colnames(X))
  xf <- .flip(x, p, shift = c(3,3.1))
  n <- .noi(x,p)
  xb <- .bl(xf)
}

.flip <- function(X, ppm, shift = c(3, 3.1)){
  iid = get_idx(shift, ppm)
  out = t(apply(X, 1, function(x, idx=iid){
    if (sum(x[idx])<0) {
      x=x*-1
    }
    return(x)
  }))
}

.noi <- function(X_OG, ppm_OG){
  rm <- apply(X_OG, 1, function(i){
    rm <- 5*sd(i[get_idx(c(9.5,11), ppm_OG)])+(mean((i[get_idx(c(9.5,11), ppm_OG)]), trim = 0.05))
    return(rm)
  })
  return(rm)
}

.bl <- function(x){
  if (is.null(ncol(x))){
    x <- t(x)
    return(x)
  }
  xb <- t(apply(x, 1, function(i){
    x <- asysm(x, maxit = 30, lambda = 1e+07)
  }))
  return(xb)
}
