#' @importFrom stringr str_replace_all
parse_prior <- function(value, name) {
  if (is.null(value) || identical(value, "null")) {
    return(NULL)
  }
  if (name %in% c("start_n", "end_n", "groups_n")) {
    parse_vec(value, name, type = "integer")
  } else if (name %in% c("start_id", "end_id", "features_id")) {
    parse_vec(value, name, type = "character")
  } else if (name %in% c("groups_id", "timecourse_discrete", "timecourse_continuous")) {
    parse_named_vec(
      value,
      names = c("cell_id", name),
      type = c("groups_id" = "character", "timecourse_discrete" = "integer", "timecourse_continuous" = "numeric")[name]
    )
  } else if (name %in% c("groups_network")) {
    parse_data_frame(
      value,
      name = name,
      types = c("from" = "character", "to" = "character")
    )
  } else {
    stop("Unrecognised prior_id")
  }
}
