#' Propose new point in the knapsack extension
#' @param x Current point
#' @param B Sampling directions (Should be a CPLB)
#' @param CPLB_idx Index of columns of matrix for A_2
#' @param a Objective function of the knapsack set definition
#' @param b Upper limit of the knapsack set definition
#'
#' @return Returns a vector containing the new proposed point in the knapsack extension
#' @export
proposePoint_knapsack <- function(x, B, CPLB_idx, a, b){
  idx = sample(1:ncol(B), 1, replace = TRUE)
  move = B[, idx]

  xmin = x - x[CPLB_idx[idx]]*move
  c_max = floor((b-(sum(x[CPLB_idx]*a) - x[CPLB_idx[idx]]*a[idx]))/a[idx])
  c = sample(0:c_max, 1, replace = TRUE)
  return(xmin+c*move)
}
