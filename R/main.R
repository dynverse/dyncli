# definition_location <- "../../methods/ti_paga/definition.yml"
# args <- c("--verbosity", "3", "--seed", "4", "--dataset", "../../methods/ti_paga/example.h5", "--output", "/ti/output/output.h5", "--n_comps", "50")

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
  # parse verbosity manually
  verbosity <-
    if (any(grepl("verbosity", args))) {
      ix <- which(grepl("verbosity", args))

      if (grepl("^--verbosity=[0-3]$", args[[ix]])) {
        as.integer(gsub(".*=", "", args[[ix]]))
      } else if (grepl("^[0-3]$", args[[ix + 1]])) {
        as.integer(args[[ix+1]])
      } else {
        stop("flag \"verbosity\" requires an argument")
      }
    } else {
      1 # default
    }
  set_verbosity(verbosity)

  debug("Parsing definition at location ", definition_location, "\n")
  definition <- dynwrap::create_ti_method_definition(filename = definition_location, return_function = FALSE)

  info("Found TI method ", definition$method$name, "\n")

  ##########################################
  ##        CREATE OPTPARSE PARSER        ##
  ##########################################
  debug("Building argument parser\n")

  parser <-
    OptionParser(usage = paste0("LOCAL=/path/to/folder; MOUNT=/ti; docker run -v $LOCAL:$MOUNT ", definition$container$docker)) %>%
    add_option("--dataset", type = "character", help = "Filename of the dataset, example: $MOUNT/dataset.(h5|loom). h5 files can be created by cellranger or dyncli.") %>%
    add_option("--loom_expression_layer", type = "character", help = "If available, the name of the loom-layer containing normalised log-transformed data.") %>%
    add_option("--output", type = "character", help = "Filename of the output trajectory data, example: $MOUNT/output.h5.") %>%

    # parameters
    add_parameter_options(definition$parameters) %>%

    # priors
    add_prior_options(definition$wrapper$inputs) %>%

    # add final parameters
    add_option("--verbosity", type = "integer", default = 1, help = "The verbosity level: 0 => none, 1 => critical (default), 2 => info, 3 => debug.") %>%
    add_option("--seed", type = "integer", default = NA, help = "A seed to be set to ensure reproducability.")

  ##########################################
  ##            PARSE ARGUMENTS           ##
  ##########################################
  debug("Parsing arguments\n")

  parsed_args <- parse_args(parser, args = args)
  debug("Arguments:\n", paste(deparse(parsed_args), collapse = "\n"), "\n")

  # process dataset object (if passed)
  info("Reading dataset file: ", parsed_args$dataset, "\n")
  task <- parse_dataset(
    parsed_args$dataset,
    loom_expression_layer = parsed_args$loom_expression_layer
  )

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
  task$priors <- task$priors %||% list()
  if (!is.null(parsed_args$priors)) {
    debug("Reading priors file: ", parsed_args$priors, "\n")
    # TODO: support hdf5
    priors_file <- yaml::read_yaml(parsed_args$priors)

    if ("groups_network" %in% names(priors_file)) {
      assert_that(priors_file$groups_network %has_names% c("from", "to"))
      priors_file$groups_network <- as_tibble(priors_file$groups_network)
    }

    for (prior_id in dynwrap::priors$prior_id %>% setdiff("dataset")) {
      debug("Reading prior: ", prior_id, "\n")
      priors_file[[prior_id]] <- parse_prior(parsed_args[[prior_id]], prior_id)
    }

    # modify existing priors on top of those provided through cli
    task$priors <- purrr::list_modify(task$priors,!!!priors_file)
  }

  # TODO: Only give required priors or those that are requested (i.e. give_priors)


  # process execution parameters
  task$verbosity <- parsed_args$verbosity
  task$seed <- parsed_args$seed
  task$output <- parsed_args$output

  if (!is.null(task$seed) && !is.na(task$seed)) set.seed(task$seed)

  info("Finished processing data\n")
  task
}
