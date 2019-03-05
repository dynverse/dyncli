#' Write TI output data to a file
#'
#' @param x A dynwrap object or a list to be passed to \code{dynwrap::wrap_output_list()}
#' @param task The task read by \code{main()}
#'
#' @export
write_output <- function(x, task) {
  if (!dynwrap::is_data_wrapper(x)) {
    x <- dynwrap::wrap_output_list(x, task$definition$wrapper$output)
  }

  write_h5(x, task$output)
}
