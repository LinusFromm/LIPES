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
    alpha = min(1, exp(w * (max(x)-max(x.star))))
  } else if(isOutside(x) && !isOutside(x.star)){
    alpha = min(1, exp(-w*max(x.star)-ldelta))
  } else if(!isOutside(x) && isOutside(x.star)){
    alpha = min(1, exp(ldelta + w*max(x)))
  } else {
    alpha = 1
  }

  return(alpha)
}
