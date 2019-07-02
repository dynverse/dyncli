# definition_location <- "../../methods/ti_cellrouter/definition.yml"
# args <- c("--verbosity", "3", "--seed", "4", "--dataset", "/tmp/example.h5", "--output", "/tmp/output.h5", "--start_id", "null")

#' dyncli main function
#'
#' @param args The arguments to be read by dyncli
#' @param definition_location The location of the definition file of the method
#'
#'
#'
#' @importFrom optparse OptionParser add_option make_option parse_args
#' @importFrom yaml read_yaml
#' @importFrom dynwrap create_ti_method_definition
#'
#' @export
main <- function(
  args = commandArgs(trailingOnly = TRUE),
  definition_location = NULL
) {
  # load definition, or find it...
  if (is.null(definition_location)) {
    if (file.exists("./definition.yml")) {
      definition_location <- "./definition.yml"
    } else if (file.exists("/code") && file.exists("/code/definition.yml")) {
      definition_location <- "/code/definition.yml"
    } else {
      stop("Definition needs to be defined")
    }
  }

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
  definition <- dynwrap::create_ti_method_definition(definition = definition_location, script = NULL, return_function = FALSE)

  parameters_default <- dynparam::get_defaults(definition$parameters)

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
    add_option("--seed", type = "integer", help = "A seed to be set to ensure reproducibility.") %>%
    add_option("--debug", action = "store_true")


  debug("Parsing arguments\n")

  parsed_args <- parse_args(parser, args = args)
  debug("Arguments:\n", paste(deparse(parsed_args), collapse = "\n"), "\n")

  # if debug, return command to debug
  if (isTRUE(parsed_args$debug)) {
    args <- args[args != "--debug"]
    args_debug <- paste0(deparse(args, width.cutoff = 500), collapse = "\n")
    command <- paste0("task <- dyncli::main(", args_debug, ")", collapse = "")
    cat("Use this command inside R to load the data: \n", crayon::bold(command), "\n")
    quit(save = "no")
  }

  ##########################################
  ##              PARSE TASK              ##
  ##########################################
  # process dataset object (if passed)
  info("Reading dataset file: ", parsed_args$dataset, "\n")
  task <- parse_dataset(
    x = parsed_args$dataset,
    loom_expression_layer = parsed_args$loom_expression_layer
  )
  debug("Parsed from dataset: ", paste(deparse(names(task)), collapse = "\n"), "\n")
  if (task %has_names% "counts") {
    info("Dataset counts dim: c(", paste(dim(task$counts), collapse = ", "), ")\n")
  }
  if (task %has_names% "expression") {
    info("Dataset expression dim: c(", paste(dim(task$expression), collapse = ", "), ")\n")
  }

  ##########################################
  ##           PARSE PARAMETERS           ##
  ##########################################
  # process parameters (if passed)
  parameters_list <-
    if (!is.null(parsed_args$parameters)) {
      debug("Reading parameters file: ", parsed_args$parameters, "\n")
      yaml::read_yaml(parsed_args$parameters)
    } else {
      list()
    }

  parameters_manual <- list()
  for (parameter in intersect(definition$parameters$parameters, names(parsed_args))) {
    debug("Reading parameter: ", parameter$id, "\n")
    parameters_manual[[parameter$id]] <- argparse_trafo(parameter, parsed_args[[parameter$id]])
  }

  task$parameters <-
    parameters_default %>%
    purrr::list_modify(!!!task$parameters) %>%
    purrr::list_modify(!!!parameters_list) %>%
    purrr::list_modify(!!!parameters_manual)

  debug("Parameters set: ", paste(deparse(task$parameters), collapse = "\n"), "\n")

  ##########################################
  ##             PARSE PRIORS             ##
  ##########################################
  # process priors (if passed)
  required_priors <-
    definition$wrapper$inputs %>%
    filter(type == "prior_information", required) %>%
    pull(input_id)
  optional_priors <-
    definition$wrapper$inputs %>%
    filter(type == "prior_information", !required) %>%
    pull(input_id)
  prior_names <- setdiff(c(required_priors, optional_priors), "dataset")

  if (!is.null(parsed_args$use_priors)) {
    if (parsed_args$use_priors == "none") {
      optional_priors <- c()
    } else if (parsed_args$use_priors == "all") {
      # do nothing
    } else {
      optional_prior_names <- parsed_args$use_priors %>% strsplit(",")
      assert_that(
        optional_prior_names %all_in% optional_priors,
        "Priors passed to --use_priors must be a subset of the method's possible optional priors. Check -h for more information."
      )
      optional_priors <- intersect(optional_priors, optional_prior_names)
    }
  }

  priors_list <-
    if (!is.null(parsed_args$priors)) {
      debug("Reading priors file: ", parsed_args$priors, "\n")
      priors_file <- yaml::read_yaml(parsed_args$priors)

      if ("groups_network" %in% names(priors_file)) {
        assert_that(priors_file$groups_network %has_names% c("from", "to"))
        priors_file$groups_network <- as_tibble(priors_file$groups_network)
      }

      priors_file
    } else {
      list()
    }

  # process specific priors, if passed
  priors_manual <- list()
  for (prior_id in intersect(prior_names, names(parsed_args))) {
    debug("Reading prior: ", prior_id, "\n")
    priors_manual[[prior_id]] <- parse_prior(parsed_args[[prior_id]], prior_id)
  }

  task$priors <-
    (task$priors %||% list()) %>%
    purrr::list_modify(!!!priors_list) %>%
    purrr::list_modify(!!!priors_manual)

  task$priors <- task$priors[intersect(names(task$priors), c(required_priors, optional_priors))]

  if (task %has_names% "priors") {
    debug("Priors passed: ", paste(names(task$priors), collapse = ", "), "\n")
  }

  ##########################################
  ##               FINALISE               ##
  ##########################################
  # process execution parameters
  task$verbosity <- parsed_args$verbosity
  task$seed <- parsed_args$seed %||% task$seed %||% NA_integer_
  task$output <- parsed_args$output
  task$debug <- parsed_args$debug

  debug("verbosity: ", task$verbosity, "\n")
  debug("seed: ", task$seed, "\n")
  debug("output: ", task$output, "\n")

  if (!is.null(task$seed) && !is.na(task$seed)) set.seed(task$seed)

  info("Finished processing data\n")

  task
}

