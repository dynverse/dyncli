#' Write TI output data to a file
#'
#' @param x A dynwrap object or a list to be passed to \code{dynwrap::wrap_output_list()}.
#' @param file Where to write the object to.
#' @param output_ids If \code{x} is a list, this is a subset of \code{dynwrap::allowed_outputs}.
#'
#' @export
write_output <- function(x, file, output_ids = NULL) {
  if (!dynwrap::is_data_wrapper(x)) {
    assert_that(!is.null(output_ids))
    x <- dynwrap::wrap_output_list(x, output_ids)
  }

  dynutils::write_h5(x, file)
}
