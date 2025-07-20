#' Find the smallest bounds of the hypercube
#' @param A Configuration matrix
#' @param y Vector of observations
#' @param CPLB_idx A2 index
#'
#' @return Vector a which defines the bounds of the hypercuboid
#' @export
hypercube_bounds <- function(A, CPLB_idx, y){
  a = rep(0, length(CPLB_idx))
  for(i in 1:length(CPLB_idx)){
    a[i] = lpSolve::lp(direction = "max",
                       objective.in = diag(ncol(A))[CPLB_idx[i],],
                       const.mat = A,
                       const.dir = "==",
                       const.rhs = y)$solution[CPLB_idx[i]]
  }
  return(a)
}
