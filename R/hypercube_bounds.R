#' Find the smallest bounds of the hypercube
#' @param A Configuration matrix
#' @param y Vector of observations
#'
#' @return Vector a which defines the bounds of the hypercuboid
#' @export
hypercube_bounds <- function(A, y){
  apply(A, 2, function(col) floor(min(y[which(col != 0)]/col[which(col != 0)])))
}
