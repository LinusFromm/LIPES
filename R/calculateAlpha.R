#' Calculate Acceptance ratio
#' @param x Current point
#' @param x.star Proposed point
#' @param ldelta log(delta) used to define epsilon
#'
#' @return Returns the Acceptance ratio
#' @export
calculateAlpha <- function(x, x.star, ldelta){
  x <- as.numeric(x)
  x.star <- as.numeric(x.star)
  ldelta <- as.numeric(ldelta)

  if(isOutside(x) && isOutside(x.star)){
    alpha = 1
  } else if(isOutside(x) && !isOutside(x.star)){
    alpha = exp(-ldelta)
  } else if(!isOutside(x) && isOutside(x.star)){
    alpha = exp(ldelta)
  } else {
    alpha = 1
  }

  return(min(1, alpha))
}
