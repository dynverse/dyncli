
#' @export
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  args <- c("ti", "--seed", "4", "--output", "/ti/output/")
  args <- c("api")

  case_when(
    length(args) == 0 ~ print_help(),
    args[[1]] == "ti" ~ main_ti(args[-1]),
    args[[1]] == "tidirect" ~
  )
}

main_api <- function(args) {
  defaults_yaml <- list(
    "function" = list(
      "name" = "ti",
      "command" = "Rscript /code/run.R",
      "pre-hook" = "/code/pre-hook.sh",
      "post-hook" = "",
      "arguments" = "",
      "parameters" = ""
    )
  )
}

print_help <- function() {

}

# definition_location <- "~/Workspace/dynverse/methods/ti_paga/definition.yml"
# args <- c("--seed", "4", "--expression", "/ti/input/expression.csv", "--counts", "/ti/input/counts.csv", "--output", "/ti/output/output.h5", "--params", "/ti/input/params.yaml", "--n_comps", "50")
#' @import optparse
#' @importFrom dynwrap create_ti_method_definition
main_ti <- function(args, definition_location = "/code/definition.yml") {
  definition <- dynwrap::create_ti_method_definition(filename = definition_location, return_function = FALSE)

  parser <-
    OptionParser(usage = paste0("docker run -v ~/ti_working_dir:/ti ", definition$container$docker)) %>%
    add_option(c("-v", "--verbose"), action = "store_true", default = FALSE, help = "Print extra output") %>%
    add_option(c("--seed"), type = "integer", default = NA, help = "A seed to be set") %>%
    add_option(c("--expression"), type = "character", help = "Filename of expression data, example: /ti/input/expression.h5") %>%
    add_option(c("--counts"), type = "character", help = "Filename of raw counts data, example: /ti/input/counts.h5") %>%
    add_option(c("--output"), type = "character", help = "Filename of the output data, example: /ti/output/output.h5") %>%
    add_option(c("--params"), type = "character", help = "A file containing method-specific parameters, example: /ti/input/params.yaml", default = NULL)

  parameters <- definition$parameters$parameters

  # generate documentation per parameter separately
  for (parameter in parameters) {
    # is there no function in optparse to do this cleanly?
    parser@options[[length(parser@options) + 1]] <-
      dynparam::as_argparse(parameter)
  }

  # generate documentation per input separately
  # definition$input
  # dynwrap::allowed_inputs

  parsed_args <- parse_args(parser, args = args)

  # convert parameter values to correct type
  param_values <- list()
  for (parameter in parameters) {
    param_values[[parameter$id]] <- dynparam::argparse_trafo(parameter, parsed_args[[parameter$id]])
  }

  param_values$verbose <- parsed_args$verbose
  param_values$seed <- parsed_args$seed

  # read expression / counts
  expression <- NULL # ...
  counts <- NULL # ...

  # read priors
  # ...

  # create dataset
  dataset <-
    dynwrap::wrap_expression(
      expression = expression,
      counts = counts
    )

  # write params to temporary file
  data <- lst(
    expression_file =
    params = param_values
  )



  ### WIP
  print(args)

  if (!is.null(args$seed) && !is.na(args$seed)) set.seed(args$seed)

  params <- list(dimred = args$dimred)
  data <- list()

  if ("expression" %in% names(args)) data$expression <- readr::read_rds(args$expression)
  if ("counts" %in% names(args)) data$counts <- readr::read_rds(args$counts)

  source("/code/run.R")

  readr::write_rds(output, args$output)
}

main_ti_with_data <- function(data_file) {

}
