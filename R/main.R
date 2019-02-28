# definition_location <- "~/Workspace/dynverse/methods/ti_paga/definition.yml"
# args <- c("--seed", "4", "--expression", "/ti/input/expression.csv", "--counts", "/ti/input/counts.csv", "--output", "/ti/output/output.h5", "--n_comps", "50")

#' dyncli main function
#'
#' @param args The arguments to be read by dyncli
#' @param definition_location The location of the definition file of the method
#'
#' @import optparse
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
    add_option(c("-v", "--verbose"), action = "store_true", default = FALSE, help = "Print extra output.") %>%
    add_option(c("--seed"), type = "integer", default = NA, help = "A seed to be set to ensure reproducability.")

  ##########################################
  ##            PARSE ARGUMENTS           ##
  ##########################################

  parsed_args <- parse_args(parser, args = args)

  # process dataset object (if passed)
  task <-
    if (!is.null(parsed_args$dataset)) {
      # TODO: support hdf5
      readr::read_rds(parsed_args$dataset)
    } else {
      list()
    }

  # process parameters (if passed)
  task$params <-
    if (!is.null(parsed_args$params)) {
      # TODO: support hdf5
      yaml::read_yaml(parsed_args$params)
    } else {
      list()
    }

  for (parameter in definition$parameters$parameters) {
    task$params[[parameter$id]] <- dynparam::argparse_trafo(parameter, parsed_args[[parameter$id]])
  }

  # process priors (if passed)
  task$priors <-
    if (!is.null(parsed_args$priors)) {
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
    task$priors[[prior_id]] <- parse_prior(parsed_args[[prior_id]], prior_id)
  }

  # process execution parameters (if passed)
  if (!is.null(parsed_args$verbose)) {
    task$verbose <- parsed_args$verbose
  }
  if (!is.null(parsed_args$seed)) {
    task$seed <- parsed_args$seed
  }

  # process expression / counts data (if passed)
  if (!is.null(parsed_args$expression)) {
    task$expression <- parse_matrix(parsed_args$expression, name = "expression", type = "numeric")
  }
  if (!is.null(parsed_args$counts)) {
    task$counts <- parse_matrix(parsed_args$counts, name = "counts", type = "integer")
  }

  task
}

add_prior_options <- function(parser, inputs) {
  poss_priors <-
    tribble(
      ~input_id, ~description, ~format, ~example,
      "start_n", "The number of start cells", "integer", "1",
      "end_n", "The number of end cells", "integer", "4",
      "groups_n", "The number of states, including the start, end and intermediary states", "integer", "5",

      "start_id", "Start cell(s); one or more start cell identifiers", "character vector", "C1,C2,C3",
      "end_id", "End cell(s); one or more end cell identifiers^", "character vector", "C1,C2,C3",
      "features_id", "A set of features known to be important in the dynamic process", "character vector", "G1,G2,G3",

      "groups_id", "Cell clustering linking cell identifiers to different states", "named character vector", "C1=A,C2=B,C3=B",
      "timecourse_discrete", "Time course (discrete), possibly from a time course experiment", "named integer vector", "C1=1,C2=4,C3=7",
      "timecourse_continuous", "Time course (continuous), possibly from a time course experiment", "named double vector", "C1=0.1,C2=0.4,C3=0.8",

      "groups_network", "State network, the known differentiation network between states", "dataframe(from: character, to: character)", "A,B;B,C;B,D"
    )

  prior_info <-
    inner_join(
      inputs,
      poss_priors,
      by = "input_id"
    )

  if (nrow(prior_info) > 0) {
    parser <- parser %>%
      add_option("--priors", type = "character", help = "A file containing prior information.\n\t\tFormat: See <....website....>.\n\t\tExample: $MOUNT/prior.(h5|yml).", default = NULL)
  }

  for (i in seq_len(nrow(prior_info))) {
    parser <- parser %>% add_option(
      opt_str = paste0("--", prior_info$input_id[[i]]),
      type = "character",
      help = paste0(
        "Prior", ifelse(prior_info$required[[i]], " (Required)", ""), ": ", prior_info$description[[i]], ".\n\t\t",
        "Format: ", prior_info$format[[i]], "\n\t\t",
        "Example: ", prior_info$example[[i]]
      )
    )
  }

  parser
}
