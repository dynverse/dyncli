# definition_location <- "~/Workspace/dynverse/methods/ti_paga/definition.yml"
# args <- c("--seed", "4", "--expression", "/ti/input/expression.csv", "--counts", "/ti/input/counts.csv", "--output", "/ti/output/output.h5", "--n_comps", "50")

#' dyncli main function
#'
#' @param args The arguments to be read by dyncli
#' @param definition_location The location of the definition file of the method
#'
#' @importFrom optparse OptionParser add_option make_option parse_args
#' @importFrom dynwrap create_ti_method_definition
#' @importFrom yaml read_yaml
#'
#' @export
main <- function(
  args = commandArgs(trailingOnly = TRUE),
  definition_location = "/code/definition.yml"
) {
  definition <- dynwrap::create_ti_method_definition(filename = definition_location, return_function = FALSE)

  ##########################################
  ##        CREATE OPTPARSE PARSER        ##
  ##########################################
  parser <-
    OptionParser(usage = paste0("LOCAL=/path/to/folder; MOUNT=/ti; docker run -v $LOCAL:$MOUNT ", definition$container$docker)) %>%
    add_option("--expression", type = "character", help = "Filename of (normalised) log-transformed expression data, example: $MOUNT/expression.(tsv|rds|h5|loom).") %>%
    add_option("--counts", type = "character", help = "Filename of raw counts data, example: $MOUNT/counts.(tsv|rds|h5|loom).") %>%
    add_option("--output", type = "character", help = "Filename of the output trajectory data, example: $MOUNT/output.(h5|rds).") %>%

    # parameters
    add_parameter_options(definition$parameters) %>%

    # priors
    add_prior_options(definition$inputs) %>%

    # add final parameters
    add_option("--verbosity", type = "integer", default = 1, help = "The verbosity level: 0 => none, 1 => critical (default), 2 => info, 3 => debug.") %>%
    add_option("--seed", type = "integer", default = NA, help = "A seed to be set to ensure reproducability.")

  ##########################################
  ##            PARSE ARGUMENTS           ##
  ##########################################

  parsed_args <- parse_args(parser, args = args)
  options(dyncli_verbosity = parsed_args$verbosity)

  # process dataset object (if passed)
  task <-
    if (!is.null(parsed_args$dataset)) {
      debug("Reading dataset file: ", parsed_args$dataset, "\n")
      # TODO: support hdf5
      readr::read_rds(parsed_args$dataset)
    } else {
      list()
    }

  # process expression / counts data (if passed)
  if (!is.null(parsed_args$expression)) {
    debug("Reading expression file: ", parsed_args$expression, "\n")
    task$expression <- parse_matrix(parsed_args$expression, name = "expression", type = "numeric")
  }
  if (!is.null(parsed_args$counts)) {
    debug("Reading counts file: ", parsed_args$counts, "\n")
    task$counts <- parse_matrix(parsed_args$counts, name = "counts", type = "integer")
  }


  # process parameters (if passed)
  task$params <-
    if (!is.null(parsed_args$params)) {
      debug("Reading params file: ", parsed_args$params, "\n")
      # TODO: support hdf5
      yaml::read_yaml(parsed_args$params)
    } else {
      list()
    }

  for (parameter in definition$parameters$parameters) {
    debug("Reading parameter: ", parameter$id, "\n")
    task$params[[parameter$id]] <- argparse_trafo(parameter, parsed_args[[parameter$id]])
  }

  # process priors (if passed)
  task$priors <-
    if (!is.null(parsed_args$priors)) {
      info("Reading priors file: ", parsed_args$priors, "\n")
      # TODO: support hdf5
      priors_file <- yaml::read_yaml(parsed_args$priors)

      if ("groups_network" %in% names(priors_file)) {
        assert_that(priors_file$groups_network %has_names% c("from", "to"))
        priors_file$groups_network <- as_tibble(priors_file$groups_network)
      }

      priors_file
    } else {
      list()
    }
  for (prior_id in dynwrap::priors$prior_id %>% setdiff("dataset")) {
    info("Reading prior: ", prior_id, "\n")
    task$priors[[prior_id]] <- parse_prior(parsed_args[[prior_id]], prior_id)
  }

  # process execution parameters
  task$verbose <- parsed_args$verbosity >= 3
  task$seed <- parsed_args$seed

  task
}
