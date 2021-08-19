#' Get ppm indexes
#'
#' @param r the lower and upper bounds of the ppm region you wish to find the indexes of
#' @param p the ppm variable you want to find the indexes of
#' @author Torben Kimhofer \email{torben.kimhofer@@murdoch.edu.au}
#' @return An array containing the indexes between the lower and upper bounds
#' @export
#'
#' @examples

get_idx <- function (r, p){
  if (length(r)>2){stop('Too many values provided. Only 2 values accepted')}
  r <- sort(r, decreasing = TRUE)
  which(p <= r[1] & p >= r[2])
}
