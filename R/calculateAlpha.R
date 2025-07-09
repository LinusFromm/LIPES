#' Calculate Acceptance ratio
#' @param x Current point
#' @param x.star Proposed point
#' @param ldelta log(delta) used to define epsilon
#' @param w Weight of negativity
#'
#' @return Returns the Acceptance ratio
#' @export
calculateAlpha <- function(x, x.star, ldelta, w = 0){
  x <- as.numeric(x)
  x.star <- as.numeric(x.star)
  ldelta <- as.numeric(ldelta)

  if(isOutside(x) && isOutside(x.star)){
    alpha = min(1, exp(w * (max(abs(x[which(x < 0)]))-max(abs(x.star[which(x.star < 0)])))))
  } else if(!isOutside(x) && isOutside(x.star)){
    alpha = min(1, exp(ldelta - w*max(abs(x.star[which(x.star < 0)]))))
  } else if(isOutside(x) && !isOutside(x.star)){
    alpha = min(1, exp(w*max(abs(x[which(x < 0)]))-ldelta))
  } else {
    alpha = 1
  }

  return(alpha)
}
