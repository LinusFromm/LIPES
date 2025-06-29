#' Removing negative rows of the output matrix
#' @param output Matrix containing the sample including chain, iteration and draw numbers.
#'
#' @return Returns sample matrix without any negative vectors.
#' @export
remove_negative_rows <- function(output) {
  # Validate input is a matrix
  if (!is.matrix(output)) {
    stop("Input must be a matrix")
  }

  # Identify rows with any negative values
  rows_to_keep <- apply(output, 1, function(row) all(row >= 0))

  # Return filtered matrix (preserve matrix structure)
  output[rows_to_keep, , drop = FALSE]
}
