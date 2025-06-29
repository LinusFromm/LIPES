#' Calculate the proportion of positive samples
#' @param output Matrix containing the sample including chain, iteration and draw numbers.
#'
#' @return Returns Returns the proportion of positive samples in the extension sample
#' @export
positive_proportion <- function(output) {
  output_pos = remove_negative_rows(output)

  return(nrow(output_pos)/nrow(output))
}
