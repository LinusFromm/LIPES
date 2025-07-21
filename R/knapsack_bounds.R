#' Find the smallest bounds of the knapsack
#' @param A Configuration matrix
#' @param y Vector of observations
#' @param CPLB_idx A2 index
#' @param objective.in Vector a of definition of the knapsack
#'
#' @return Vector a which defines the bounds of the hypercuboid
#' @export
knapsack_bounds <- function(A, CPLB_idx, objective.in, y){
  obj = rep(0, ncol(A))
  obj[CPLB_idx] = objective.in
  obj[-CPLB_idx] = 0

  b = sum(lpSolve::lp(direction = "max",
                  objective.in = obj,
                  const.mat = A,
                  const.dir = "==",
                  const.rhs = y,
                  int.vec = 1:ncol(A))$solution[CPLB_idx]*objective.in)
  return(b)
}
