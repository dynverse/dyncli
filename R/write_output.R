#' Write a dynwrap object to a file
#'
#' A data object created by chaining `wrap_data(...)`
#' together with several `add_x(...)` function calls.
#'
#' @param x A dynwrap object.
#' @param file Where to write the object to.
#'
#' @export
#'
#' @examples
#' library(dynwrap)
#' library(purrr)
#'
#' # generate some toy single-cell data
#' cell_ids <- paste0("Cell", seq_len(10))
#'
#' # pseudotime from origin
#' pseudotime <- runif(10)
#' names(pseudotime) <- cell_ids
#'
#' # clustering
#' grouping <- sample(letters[1:3], 10, replace = TRUE)
#' names(grouping) <- cell_ids
#'
#' # create a dynwrap object
#' traj <- wrap_data(
#'   cell_ids = cell_ids
#' ) %>% add_linear_trajectory(
#'   pseudotime = pseudotime
#' ) %>% add_grouping(
#'   grouping = grouping
#' )
#'
#' # write output to a temporary file
#' tmp <- tempfile()
#' write_output(traj, tmp)
#'
#' # clean up
#' file.remove(tmp)
write_output <- function(x, file) {
  assert_that(dynwrap::is_data_wrapper(x))

  dynutils::write_h5(x, file)
}
