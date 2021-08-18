#' Title
#'
#' @param x
#' @param p
#' @param c3
#' @param c4
#' @param n
#'
#' @return
#' @export
#'
#' @examples

crea <- function(x, p, n, c3 = c(3,3.1), c4 = c(4,4.1)){
  x3 <- x[1,shift_pickr(x, p, c3, 0.005)]
  x3[x3<n]=0
  a3 <- sum(x3)
  x4 <- x[1,shift_pickr(x, p, c4, 0.005)]
  x4[x4<n]=0
  a4 <- sum(x4)
  a <- sum(a3, a4)
  r <- a4/a3
  er <- ((2/3)/100)*10
  lo <- (2/3)-er
  up <- (2/3)+er
  e <- as.array(r<=up & r>=lo)
  return(a*2)
}

#removed indexs but not sure if i need them
