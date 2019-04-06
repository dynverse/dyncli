add_parameter_options <- function(parser, par_set) {
  debug("Building method parameter parsers\n")
  if (length(par_set$parameters) > 0) {
    parser <- parser %>% add_option("--parameters", type = "character", help = "A file containing method-specific parameters, example: $MOUNT/parameters.yml.", default = NULL)
  }
  for (parameter in par_set$parameters) {
    # debug("Building parser for parameter ", parameter$id, "\n")
    parser@options[[length(parser@options) + 1]] <- as_argparse(parameter)
  }
  parser
}

as_argparse <- function(x) {
  UseMethod("as_argparse")
}

#' @importFrom optparse make_option
as_argparse.parameter <- function(x) {
  lis <- as_descriptive_tibble(x) %>% unlist()

  optparse::make_option(
    opt_str = paste0("--", x$id),
    type = "character",
    help = paste0("Parameter: ", get_description(x, sep = "\n\t\t"))
  )
}


argparse_trafo <- function(x, v) {
  UseMethod("argparse_trafo")
}

argparse_trafo.parameter <- function(x, v) {
  strsplit(v, split = ",") %>% first()
}

argparse_trafo.integer_parameter <- function(x, v) {
  argparse_trafo.parameter(x, v) %>% as.integer()
}

argparse_trafo.logical_parameter <- function(x, v) {
  vec <- argparse_trafo.parameter(x, v) %>% tolower()

  case_when(
    vec %in% c("t", "true", "yes", "y", "on") ~ TRUE,
    vec %in% c("f", "false", "no", "n", "off") ~ FALSE,
    TRUE ~ NA
  )
}

argparse_trafo.range_parameter <- function(x, v) {
  argparse_trafo.parameter(x, v) %>% as.numeric()
}

argparse_trafo.numeric_parameter <- function(x, v) {
  argparse_trafo.parameter(x, v) %>% as.numeric()
}
