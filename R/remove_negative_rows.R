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
  output_pos = output[rows_to_keep, , drop = FALSE]

  pos = matrix(NA, nrow = nrow(output_pos), ncol = ncol(output_pos))
  pos[,1:(ncol(output_pos)-2)] = output_pos[,1:(ncol(output_pos)-2)]

  for(i in 1:max(output[,ncol(output)-2])){
    pos[,(ncol(output_pos)-1)] = seq(1, length(which(output_pos[,ncol(output_pos)-2] == i)))
  }
  pos[,ncol(output_pos)] = 1:nrow(pos)

  return(pos)
}
