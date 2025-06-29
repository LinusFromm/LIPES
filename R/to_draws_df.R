#' Converting from matrix to draws_df
#' @param output Matrix containing the sample including chain, iteration and draw numbers.
#' @param var.names Fill names for the variables if NULL names will be $xi$ for the ith variable.
#'
#' @return Returns the sample object of any of the samplers as a draws_df file
#' @export
to_draws_df <- function(output, var.names = NULL){
  output_df = as.data.frame(output)

  if(is.null(var.names)){
    colnames(output_df) <- c(paste0("x",1:(ncol(output)-3)), ".chain", ".iteration", ".draw")
  } else {
    colnames(output_df) <- c(var.names, ".chain", ".iteration", ".draw")
  }

  return(posterior::as_draws_df(output_df))
}
